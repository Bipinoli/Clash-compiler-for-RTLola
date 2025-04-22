{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module CycleComplexLoop where

import Clash.Prelude

---------------------------------------------------------------

-- specification:

-- input a : Int8 
-- input b : Int8
-- output c := a + b + d.offset(by: -1).defaults(to:3)
-- output d := b + e.offset(by:-2).defaults(to:4)
-- output e := a.offset(by:-1).defaults(to:1) + c
-- output f := a + a.offset(by: -1).defaults(to: 0)
-- output g := b + f
-- output time_stream @1kHz := a.hold().defaults(to:6)

-------------------------------------
-- |   stream    |  layer |  pacing  | datatype
-------------------------------------
-- |     c       |   1    |  a & b   | Int8
-- |     d       |   1    |  a & b   | Int8
-- |     e       |   2    |  a & b   | Int8
-- |     f       |   1    |    a     | Int8
-- |     g       |   2    |  a & b   | Int8
-- | time_stream |   1    |   1Hz    | Int8
-------------------------------------

type Data = Signed 8
type DataA = Data
type DataB = Data
type DataC = Data
type DataD = Data
type DataE = Data
type DataF = Data
type DataG = Data
type DataTimeStream = Data
type HasData = (Data, Bool)
type Inputs = (HasData, HasData)

type Outputs = (HasData, HasData, HasData, HasData, HasData, HasData)

class Pacing a where
    unwrap :: a -> Bool

newtype PacingA = PacingA Bool deriving (Generic, NFDataX)
instance Pacing PacingA where
    unwrap (PacingA x) = x

newtype PacingB = PacingB Bool deriving (Generic, NFDataX)
instance Pacing PacingB where
    unwrap (PacingB x) = x

newtype PacingAAndB = PacingAAndB Bool deriving (Generic, NFDataX)
instance Pacing PacingAAndB where
    unwrap (PacingAAndB x) = x

newtype Pacing1Hz = Pacing1Hz Bool deriving (Generic, NFDataX)
instance Pacing Pacing1Hz where
    unwrap (Pacing1Hz x) = x

mkPacingAAndB :: PacingA -> PacingB -> PacingAAndB
mkPacingAAndB a b = PacingAAndB (unwrap a && unwrap b)


dataDeflt = 0 :: Data



---------------------------------------------------------------
-- Total odering of events maintained in the queue

type Pacings = (PacingA, PacingAAndB, Pacing1Hz)
type Event = (Inputs, Pacings)
-- existing architecture keeps event-based and periodic events separately -> alernate one after another

nullEvent :: Event
nullEvent = (((0, False), (0, False)), (PacingA False, PacingAAndB False, Pacing1Hz False))
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

        newEvent = newInput .||. newPacing
        newInput = newA .||. newB
        newPacing = unwrap <$> pacingA .||. unwrap <$> pacingAAndB .||. unwrap <$> pacing1Hz

        event = bundle (inputs, pacings)
        pacings = bundle (pacingA, pacingAAndB, pacing1Hz)

        (hasDataA, hasDataB) = unbundle inputs
        (_, newA) = unbundle hasDataA
        (_, newB) = unbundle hasDataB
        pacingA = PacingA <$> newA
        pacingB = PacingB <$> newB
        pacingAAndB = mkPacingAAndB <$> pacingA <*> pacingB

        pacing1Hz = Pacing1Hz <$> timer1Over
        timer1Over = timer1 .>=. period1Ns
        timer1 = timer timer1Over
        period1Ns = 1000000 -- 1kHz in ns

        timer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int
        timer reset = register 0 (mux reset (pure deltaTime) nextTime)
            where 
                nextTime = timer reset + pure deltaTime
                deltaTime = systemClockPeriodNs


---------------------------------------------------------------

data State = StatePop | StateRead | StateEvalLayer0 | StateEvalLayer1 | StateEvalLayer2  | StateOutput
    deriving (Generic, Eq, NFDataX)

llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom (Bool, Outputs, (State, DataA, DataB, Pacing1Hz))
llc event = bundle (toPop, outputs, debugSignals)
    where 
        state = register StatePop nextStateSignal

        -- state: pop
        toPop = state .==. (pure StatePop)
        (isValidEvent, poppedEvent) = unbundle event
        eventInfo = register nullEvent (mux isValidEvent poppedEvent eventInfo)
        (inputs, pacings) = unbundle eventInfo
        (hasA, hasB) = unbundle inputs
        (a, _) = unbundle hasA
        (b, _) = unbundle hasB
        (pacingA, pacingAAndB, pacing1Hz) = unbundle pacings

        -- state: layer 0
        winA = streamA (state .==. (pure StateEvalLayer0)) a

        -- state: layer 1
        evalC = streamC (state .==. (pure StateEvalLayer1) .&&. (unwrap <$> pacingAAndB)) a b evalD
        evalD = streamD (state .==. (pure StateEvalLayer1) .&&. (unwrap <$> pacingAAndB)) b evalE
        evalF = streamF (state .==. (pure StateEvalLayer1) .&&. (unwrap <$> pacingA)) a winA
        evalTS = streamTimeStream (state .==. (pure StateEvalLayer1) .&&. (unwrap <$> pacing1Hz)) winA

        -- state: layer 2
        evalE = streamE (state .==. (pure StateEvalLayer2) .&&. (unwrap <$> pacingAAndB)) winA evalC
        evalG = streamG (state .==. (pure StateEvalLayer2) .&&. (unwrap <$> pacingAAndB)) b evalF

        -- state: output
        outAktvCDEG = state .==. (pure StateOutput) .&&. (unwrap <$> pacingAAndB)
        outAktvF = state .==. (pure StateOutput) .&&. (unwrap <$> pacingA)
        outAktvTS = state .==. (pure StateOutput) .&&. (unwrap <$> pacing1Hz)

        outputs = bundle (outputC, outputD, outputE, outputF, outputG, outputTS)
        outputC = bundle (evalC, outAktvCDEG)
        outputD = bundle (dOut, outAktvCDEG)
        outputE = bundle (last <$> eOut, outAktvCDEG)
        outputF = bundle (evalF, outAktvF)
        outputG = bundle (evalG, outAktvCDEG)
        outputTS = bundle (evalTS, outAktvTS)

        (dOut, dValid) = unbundle evalD
        (eOut, eValid) = unbundle evalE

        -- state transition
        nextStateSignal = nextState <$> state <*> isValidEvent
        
        nextState :: State -> Bool -> State
        nextState curState validEvent = case curState of
            StatePop -> StateRead
            StateRead -> if validEvent then StateEvalLayer0 else StatePop 
            StateEvalLayer0 -> StateEvalLayer1
            StateEvalLayer1 -> StateEvalLayer2
            StateEvalLayer2 -> StateOutput
            StateOutput -> StatePop

        debugSignals = bundle (state, a, b, pacing1Hz)


streamA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataA -> Signal dom (Vec 2 DataA, Vec 2 Bool)
streamA en a = bundle (dataOut, validOut)
    where 
        dataOut = register (repeat dataDeflt) (mux en ((<<+) <$> dataOut <*> a) dataOut)
        validOut = register (repeat False) (mux en ((<<+) <$> validOut <*> pure True) validOut)

streamC :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataA -> Signal dom DataB -> Signal dom (DataD, Bool) -> Signal dom DataC
streamC en a b winD = out
    where 
        out = register dataDeflt (mux en nextOut out)
        nextOut = a + b + (mux offsetValid offsetData defaultData)

        defaultData = pure 3 :: Signal dom DataD
        (offsetData, offsetValid) = unbundle winD

streamD :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataB -> Signal dom (Vec 2 DataE, Vec 2 Bool) -> Signal dom (DataD, Bool)
streamD en b winE = bundle (out, outValid)
    where 
        out = register dataDeflt (mux en nextOut out)
        outValid = register False (mux en (pure True) outValid)
        nextOut = b + (mux offsetValid offsetData defaultData)

        defaultData = pure 4 :: Signal dom DataE
        offsetIndex = 0
        (dataE, validE) = unbundle winE
        offsetData = (!!) <$> dataE <*> pure offsetIndex
        offsetValid = (!!) <$> validE <*> pure offsetIndex

streamE :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Vec 2 DataA, Vec 2 Bool) -> Signal dom DataC -> Signal dom (Vec 2 DataE, Vec 2 Bool)
streamE en winA c = bundle (out, outValid)
    where 
        out = register (repeat dataDeflt) (mux en ((<<+) <$> out <*> nextRslt) out)
        outValid = register (repeat False) (mux en ((<<+) <$> outValid <*> pure True) outValid)
        nextRslt = (mux offsetValid offsetData defaultData) + c

        defaultData = pure 1 :: Signal dom DataE
        offsetIndex = 0
        (dataA, validA) = unbundle winA
        offsetData = (!!) <$> dataA <*> pure offsetIndex
        offsetValid = (!!) <$> validA <*> pure offsetIndex

streamF :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataA -> Signal dom (Vec 2 DataA, Vec 2 Bool) -> Signal dom DataF
streamF en a winA = out
    where 
        out = register dataDeflt (mux en nextOut out)
        nextOut = a + (mux offsetValid offsetData defaultData)
        defaultData = pure 0 :: Signal dom DataA
        offsetIndex = 0
        (dataA, validA) = unbundle winA
        offsetData = (!!) <$> dataA <*> pure offsetIndex
        offsetValid = (!!) <$> validA <*> pure offsetIndex

streamG :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataB -> Signal dom DataF -> Signal dom DataG
streamG en b f = out
    where 
        out = register dataDeflt (mux en nextOut out)
        nextOut = b + f

streamTimeStream :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Vec 2 DataA, Vec 2 Bool) -> Signal dom DataTimeStream
streamTimeStream en winA = out
    where 
        out = register dataDeflt (mux en nextOut out)
        nextOut = mux offsetValid offsetData defaultData
        (dataA, validA) = unbundle winA
        offsetData = last <$> dataA
        offsetValid = last <$> validA
        defaultData = pure 6 :: Signal dom DataA



---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Outputs, (Bool, Bool, Bool, Bool, State, Data, Data, Pacing1Hz))
monitor inputs = bundle (outputs, debugSignals)
    where 
        (qPushValid, qPopValid, qPopData) = unbundle (queue (bundle (qPush, qPop, qInptData)))

        (newEvent, event) = unbundle (hlc inputs)
        qPush = newEvent
        qInptData = event

        (toPop, outputs, llcDebugInfo) = unbundle (llc (bundle (qPopValid, qPopData)))
        qPop = toPop

        -- debug signals
        debugSignals = bundle (qPush, qPop, qPushValid, qPopValid, llcState, llcA, llcB, pacing1Hz)
        (llcState, llcA, llcB, pacing1Hz) = unbundle llcDebugInfo


---------------------------------------------------------------


topEntity :: Clock MyDomain -> Reset MyDomain -> Enable MyDomain -> 
    Signal MyDomain Inputs -> Signal MyDomain (Outputs, (Bool, Bool, Bool, Bool, State, Data, Data, Pacing1Hz))
topEntity clk rst en input0 = exposeClockResetEnable (monitor input0) clk rst en

