{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module CyclePipelineNotPipelineBoth where

import Clash.Prelude

---------------------------------------------------------------

-- specification:

-- input x : Int32
-- // part that can't be pipelined
-- output a := x + c.offset(by: -1).defaults(to: 0)
-- output b := a + 1
-- output c := b + 1
-- // part that can be pipelined
-- output d := x + f.offset(by: -4).defaults(to: 0)
-- output e := d + 1
-- output f := e + 1


newtype DataX = DataX Int deriving (Generic, NFDataX, Show)

unwrapDataX :: DataX -> Int
unwrapDataX (DataX x) = x

type HasDataX = (DataX, Bool)
type Inputs = HasDataX

newtype PacingABCDEF = PacingABCDEF Bool deriving (Generic, NFDataX)

unwrapPacingABCDEF :: PacingABCDEF -> Bool
unwrapPacingABCDEF (PacingABCDEF x) = x

aDflt = DataX 0
bDflt = DataX 0
cDflt = DataX 0
dDflt = DataX 0
eDflt = DataX 0
fDflt = DataX 0

type Outputs = (HasDataX, HasDataX, HasDataX)


---------------------------------------------------------------
-- Total odering of events maintained in the queue

type Pacings = PacingABCDEF
type Event = (Inputs, Pacings)
-- existing architecture keeps event-based and periodic events separately -> alernate one after another

nullEvent :: Event
nullEvent = ((DataX 0, False), PacingABCDEF False)
type QMemSize = 6

type QData = Event
type QMem = Vec QMemSize QData
type QCursor = Int
type QPush = Bool
type QPop = Bool
type QPushValid = Bool
type QPopValid = Bool

type QState = (QMem, QCursor)
type QInput = (QPush, QPop, QData)
type QOutput = (QPushValid, QPopValid, QData)

queue :: HiddenClockResetEnable dom => Signal dom QInput -> Signal dom QOutput
queue input = output
    where 
        -- keeping in registers to avoid any combinational output
        output = bundle (pushValid, popValid, outData)
        state = bundle (buffer, cursor)
        buffer = register (repeat nullEvent :: QMem) nextBufferSignal
        cursor = register 0 nextCursorSignal
        pushValid = register False nextPushValidSignal
        popValid = register False nextPopValidSignal
        outData = register nullEvent nextOutDataSignal

        -- Signal is an applicative functor
        -- <$> = alias for fmap - wraps normal function into a function on Signal
        -- <*> = apply applicative - it applies signal of function to a signal of a value

        nextBufferSignal = nextBuffer <$> buffer <*> bundle (input, cursor)
        nextCursorSignal = nextCursor <$> cursor <*> bundle (input, buffer)
        nextOutDataSignal = nextOutData <$> bundle (input, cursor, buffer)
        nextPushValidSignal = nextPushValid <$> bundle (input, cursor, buffer)
        nextPopValidSignal = nextPopValid <$> bundle (input, cursor)
        
        nextBuffer :: QMem -> (QInput, QCursor) -> QMem
        nextBuffer buf ((push, pop, qData), cur) = out
            where 
                out = case (push, pop) of
                    (True, _) -> if cur /= length buf then qData +>> buf else buf
                    (_, _) -> buf

        nextCursor :: QCursor -> (QInput, QMem) -> QCursor
        nextCursor cur ((push, pop, _), buf) = out
            where 
                out = case (push, pop) of
                    (True, False) -> if cur /= length buf then cur + 1 else cur
                    (False, True) -> if cur /= 0 then cur - 1 else 0
                    (True, True) -> if cur == 0 then cur + 1 else cur 
                    (_, _) -> cur

        nextOutData :: (QInput, QCursor, QMem) -> QData
        nextOutData ((push, pop, _), cur, buf) = out
            where 
                out = case (push, pop) of
                    (_, True) -> if cur == 0 then nullEvent else buf !! (cur - 1)
                    (_, _) -> nullEvent

        nextPushValid :: (QInput, QCursor, QMem) -> QPush
        nextPushValid ((push, pop, _), cur, buf) = out
            where 
                out = case (push, pop) of
                    (True, _) -> cur /= length buf
                    (_, _) -> False

        nextPopValid :: (QInput, QCursor) -> QPop
        nextPopValid ((push, pop, _), cur) = out
            where 
                out = case (push, pop) of
                    (_, True) -> cur /= 0
                    (_, _) -> False

---------------------------------------------------------------

-- Create a clock domain with 2 microseconds period (500 kHz) to match the testbench
createDomain vSystem{vName="MyDomain", vPeriod=2000} -- period in nanoseconds

systemClockPeriodNs :: Int
systemClockPeriodNs = fromInteger (snatToInteger $ clockPeriod @MyDomain)


hlc :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Bool, Event)
hlc inputs = out
    where 
        out = bundle (newEvent, event)
        newEvent = newX .||. (unwrapPacingABCDEF <$> pacingABCDEF)
        event = bundle (inputs, pacings)
        pacings = pacingABCDEF

        (_, newX) = unbundle inputs
        pacingABCDEF = PacingABCDEF <$> newX

---------------------------------------------------------------
---------------- LLC that can be pipelined --------------------

pipelineLLC :: HiddenClockResetEnable dom => Signal dom Event -> Signal dom (Outputs, Vec 2 DataX)
pipelineLLC event = bundle (outputs, debugSignals)
    where 
        (inputs, pacings) = unbundle event
        inputX = inputs
        pacingDEF = pacings

        (x, _) = unbundle inputX

        pacingD = unwrapPacingABCDEF <$> pacingDEF
        pacingE = delay False pacingD
        pacingF = delay False pacingE

        evalD = streamD pacingD x evalF 
        evalE = streamE pacingE evalD
        evalF = streamF pacingF evalE

        outAktvD = delay False (unwrapPacingABCDEF <$> pacingDEF)
        outAktvE = delay False outAktvD
        outAktvF = delay False outAktvE

        outputs = bundle (outputD, outputE, outputF)
        outputD = bundle (evalD, outAktvD)
        outputE = bundle (evalE, outAktvE)
        outputF = bundle (last <$> evalF, outAktvF)

        --- debug infos
        debugSignals = evalF
        


-- Note: due to propagation time of 3 cycles, stream f is always at -3 compared to stream d 
-- i.e current f = -3 offset, so we should store one value in past for offset of -4 -> window 2
streamD :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom (Vec 2 DataX) -> Signal dom DataX
streamD en x winF = out
    where 
        out = register dDflt (mux en (operate <$> x <*> winF) out)
        operate :: DataX -> Vec 2 DataX -> DataX
        operate _x _winF = DataX (unwrapDataX _x + unwrapDataX (head _winF))

streamE :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom DataX
streamE en d = out
    where 
        out = register eDflt (mux en (operate <$> d) out)
        operate :: DataX -> DataX
        operate _d = DataX (unwrapDataX _d + 1)

streamF :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom (Vec 2 DataX)
streamF en e = out
    where 
        out = register (repeat fDflt) (mux en (operate <$> out <*> e) out)
        operate :: Vec 2 DataX -> DataX -> Vec 2 DataX
        operate winF _e = winF <<+ DataX (unwrapDataX _e + 1)



-- ---------------------------------------------------------------

-- data State = StatePop | StateRead |  StateEvalLayer1 | StateEvalLayer2  | StateEvalLayer3 | StateOutput
--     deriving (Generic, Eq, NFDataX)

-- llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom (Bool, Outputs, (State, DataX, PacingABCDEF))
-- llc event = bundle (toPop, outputs, debugSignals)
--     where 
--         state = register StatePop nextStateSignal

--         -- state: pop
--         toPop = state .==. (pure StatePop)
--         (isValidEvent, poppedEvent) = unbundle event
--         eventInfo = register nullEvent (mux isValidEvent poppedEvent eventInfo)
--         (hasDataX, pacingABCDEF) = unbundle eventInfo
--         (x, newX) = unbundle hasDataX

--         -- state: layer 1
--         evalA = evaluateA (state .==. (pure StateEvalLayer1) .&&. (unwrapPacingABCDEF <$> pacingABCDEF)) x evalCWin

--         -- state: layer 2
--         evalB = evaluateB (state .==. (pure StateEvalLayer2) .&&. (unwrapPacingABCDEF <$> pacingABCDEF)) evalA

--         -- state: layer 3
--         evalCWin = evaluateC (state .==. (pure StateEvalLayer3) .&&. (unwrapPacingABCDEF <$> pacingABCDEF)) evalB

--         -- state: output
--         outAktvABC = state .==. (pure StateOutput) .&&. (unwrapPacingABCDEF <$> pacingABCDEF)

--         outputs = bundle (outputA, outputB, outputC)
--         outputA = bundle (evalA, outAktvABC)
--         outputB = bundle (evalB, outAktvABC)
--         outputC = bundle (last <$> evalCWin, outAktvABC)

--         -- state transition
--         nextStateSignal = nextState <$> state <*> isValidEvent
        
--         nextState :: State -> Bool -> State
--         nextState curState validEvent = case curState of
--             StatePop -> StateRead
--             StateRead -> if validEvent then StateEvalLayer1 else StatePop 
--             StateEvalLayer1 -> StateEvalLayer2
--             StateEvalLayer2 -> StateEvalLayer3
--             StateEvalLayer3 -> StateOutput
--             StateOutput -> StatePop

--         debugSignals = bundle (state, x, pacingABCDEF)



-- evaluateA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom (Vec 2 DataX) -> Signal dom DataX
-- evaluateA en x winC = out
--     where 
--         out = register aDflt (mux en (operate <$> x <*> winC) out)
--         operate :: DataX -> Vec 2 DataX -> DataX
--         operate d win = DataX (unwrapDataX d + unwrapDataX (head win))


-- evaluateB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom DataX
-- evaluateB en a = out
--     where 
--         out = register bDflt (mux en (operate <$> a) out)
--         operate :: DataX -> DataX
--         operate d = DataX (unwrapDataX d + 1)


-- evaluateC :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom (Vec 2 DataX)
-- evaluateC en b = out
--     where 
--         out = register (repeat cDflt) (mux en (operate <$> out <*> b) out)

--         operate :: Vec 2 DataX -> DataX -> Vec 2 DataX
--         operate winC d = winC <<+ DataX (unwrapDataX d + 1) 




---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Outputs, Vec 2 DataX)
monitor inputs = bundle (outputs, debugSignals)
    where 
        (newEvent, event) = unbundle (hlc inputs)
        (outputs, debugSignals) = unbundle (pipelineLLC event)



-- monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Outputs, (State, Bool, Bool, Bool, Bool, DataX, PacingABCDEF))
-- monitor inputs = bundle (outputs, debugSignals)
--     where 
--         (qPushValid, qPopValid, qPopData) = unbundle (queue (bundle (qPush, qPop, qInptData)))

--         (newEvent, event) = unbundle (hlc inputs)
--         qPush = newEvent
--         qInptData = event

--         (toPop, outputs, llcDebugInfo) = unbundle (llc (bundle (qPopValid, qPopData)))
--         qPop = toPop

--         -- debug signals
--         debugSignals = bundle (llcState, qPush, qPop, qPushValid, qPopValid, x, pacingABCDEF)
--         (llcState, x, pacingABCDEF) = unbundle llcDebugInfo


-- ---------------------------------------------------------------


topEntity :: Clock MyDomain -> Reset MyDomain -> Enable MyDomain -> 
    Signal MyDomain Inputs -> Signal MyDomain (Outputs, Vec 2 DataX)
topEntity clk rst en input0 = exposeClockResetEnable (monitor input0) clk rst en

