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
-- output d := x + e.offset(by: -3).defaults(to: 0)
-- output e := d + 1
-- output f := e + 1

-- Note: 
-- For pipeline case
-- All evalution happen in a single cycle
-- However the propagation takes time so we should activate output with delay
-- Also -3 offset of c needed by a in a -> b -> c -> a -> b -> c would be -1 offset of current c as c is 2 setps away

-- newtype DataX = DataX Int deriving (Generic, NFDataX, Show)

-- unwrapDataX :: DataX -> Int
-- unwrapDataX (DataX x) = x

-- type HasDataX = (DataX, Bool)
-- type Inputs = HasDataX

-- newtype PacingABC = PacingABC Bool deriving (Generic, NFDataX)

-- unwrapPacingABC :: PacingABC -> Bool
-- unwrapPacingABC (PacingABC x) = x

-- aDflt = DataX 0
-- bDflt = DataX 0
-- cDflt = DataX 0

-- type Outputs = (HasDataX, HasDataX, HasDataX)


-- ---------------------------------------------------------------
-- -- Total odering of events maintained in the queue

-- type Pacings = PacingABC
-- type Event = (Inputs, Pacings)
-- -- existing architecture keeps event-based and periodic events separately -> alernate one after another

-- nullEvent :: Event
-- nullEvent = ((DataX 0, False), PacingABC False)
-- type QMemSize = 6

-- type QData = Event
-- type QMem = Vec QMemSize QData
-- type QCursor = Int
-- type QPush = Bool
-- type QPop = Bool
-- type QPushValid = Bool
-- type QPopValid = Bool

-- type QState = (QMem, QCursor)
-- type QInput = (QPush, QPop, QData)
-- type QOutput = (QPushValid, QPopValid, QData)

-- queue :: HiddenClockResetEnable dom => Signal dom QInput -> Signal dom QOutput
-- queue input = output
--     where 
--         -- keeping in registers to avoid any combinational output
--         output = bundle (pushValid, popValid, outData)
--         state = bundle (buffer, cursor)
--         buffer = register (repeat nullEvent :: QMem) nextBufferSignal
--         cursor = register 0 nextCursorSignal
--         pushValid = register False nextPushValidSignal
--         popValid = register False nextPopValidSignal
--         outData = register nullEvent nextOutDataSignal

--         -- Signal is an applicative functor
--         -- <$> = alias for fmap - wraps normal function into a function on Signal
--         -- <*> = apply applicative - it applies signal of function to a signal of a value

--         nextBufferSignal = nextBuffer <$> buffer <*> bundle (input, cursor)
--         nextCursorSignal = nextCursor <$> cursor <*> bundle (input, buffer)
--         nextOutDataSignal = nextOutData <$> bundle (input, cursor, buffer)
--         nextPushValidSignal = nextPushValid <$> bundle (input, cursor, buffer)
--         nextPopValidSignal = nextPopValid <$> bundle (input, cursor)
        
--         nextBuffer :: QMem -> (QInput, QCursor) -> QMem
--         nextBuffer buf ((push, pop, qData), cur) = out
--             where 
--                 out = case (push, pop) of
--                     (True, _) -> if cur /= length buf then qData +>> buf else buf
--                     (_, _) -> buf

--         nextCursor :: QCursor -> (QInput, QMem) -> QCursor
--         nextCursor cur ((push, pop, _), buf) = out
--             where 
--                 out = case (push, pop) of
--                     (True, False) -> if cur /= length buf then cur + 1 else cur
--                     (False, True) -> if cur /= 0 then cur - 1 else 0
--                     (True, True) -> if cur == 0 then cur + 1 else cur 
--                     (_, _) -> cur

--         nextOutData :: (QInput, QCursor, QMem) -> QData
--         nextOutData ((push, pop, _), cur, buf) = out
--             where 
--                 out = case (push, pop) of
--                     (_, True) -> if cur == 0 then nullEvent else buf !! (cur - 1)
--                     (_, _) -> nullEvent

--         nextPushValid :: (QInput, QCursor, QMem) -> QPush
--         nextPushValid ((push, pop, _), cur, buf) = out
--             where 
--                 out = case (push, pop) of
--                     (True, _) -> cur /= length buf
--                     (_, _) -> False

--         nextPopValid :: (QInput, QCursor) -> QPop
--         nextPopValid ((push, pop, _), cur) = out
--             where 
--                 out = case (push, pop) of
--                     (_, True) -> cur /= 0
--                     (_, _) -> False

-- ---------------------------------------------------------------

-- -- Create a clock domain with 2 microseconds period (500 kHz) to match the testbench
-- createDomain vSystem{vName="MyDomain", vPeriod=2000} -- period in nanoseconds

-- systemClockPeriodNs :: Int
-- systemClockPeriodNs = fromInteger (snatToInteger $ clockPeriod @MyDomain)


-- hlc :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Bool, Event)
-- hlc inputs = out
--     where 
--         out = bundle (newEvent, event)
--         newEvent = newX .||. (unwrapPacingABC <$> pacingABC)
--         event = bundle (inputs, pacings)
--         pacings = pacingABC

--         (_, newX) = unbundle inputs
--         pacingABC = PacingABC <$> newX


-- ---------------------------------------------------------------

-- data State = StatePop | StateRead |  StateEvalLayer1 | StateEvalLayer2  | StateEvalLayer3 | StateOutput
--     deriving (Generic, Eq, NFDataX)

-- llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom (Bool, Outputs, (State, DataX, PacingABC))
-- llc event = bundle (toPop, outputs, debugSignals)
--     where 
--         state = register StatePop nextStateSignal

--         -- state: pop
--         toPop = state .==. (pure StatePop)
--         (isValidEvent, poppedEvent) = unbundle event
--         eventInfo = register nullEvent (mux isValidEvent poppedEvent eventInfo)
--         (hasDataX, pacingABC) = unbundle eventInfo
--         (x, newX) = unbundle hasDataX

--         -- state: layer 1
--         evalA = evaluateA (state .==. (pure StateEvalLayer1) .&&. (unwrapPacingABC <$> pacingABC)) x evalCWin

--         -- state: layer 2
--         evalB = evaluateB (state .==. (pure StateEvalLayer2) .&&. (unwrapPacingABC <$> pacingABC)) evalA

--         -- state: layer 3
--         evalCWin = evaluateC (state .==. (pure StateEvalLayer3) .&&. (unwrapPacingABC <$> pacingABC)) evalB

--         -- state: output
--         outAktvABC = state .==. (pure StateOutput) .&&. (unwrapPacingABC <$> pacingABC)

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

--         debugSignals = bundle (state, x, pacingABC)



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




-- ---------------------------------------------------------------

-- monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Outputs, (State, Bool, Bool, Bool, Bool, DataX, PacingABC))
-- monitor inputs = bundle (outputs, debugSignals)
--     where 
--         (qPushValid, qPopValid, qPopData) = unbundle (queue (bundle (qPush, qPop, qInptData)))

--         (newEvent, event) = unbundle (hlc inputs)
--         qPush = newEvent
--         qInptData = event

--         (toPop, outputs, llcDebugInfo) = unbundle (llc (bundle (qPopValid, qPopData)))
--         qPop = toPop

--         -- debug signals
--         debugSignals = bundle (llcState, qPush, qPop, qPushValid, qPopValid, x, pacingABC)
--         (llcState, x, pacingABC) = unbundle llcDebugInfo


-- ---------------------------------------------------------------


-- topEntity :: Clock MyDomain -> Reset MyDomain -> Enable MyDomain -> 
--     Signal MyDomain Inputs -> Signal MyDomain (Outputs, (State, Bool, Bool, Bool, Bool, DataX, PacingABC))
-- topEntity clk rst en input0 = exposeClockResetEnable (monitor input0) clk rst en

