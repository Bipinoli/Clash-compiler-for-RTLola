{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module SlidingWindowSimple where

import Clash.Prelude
import Data.Coerce

---------------------------------------------------------------

-- specification:

-- input x : Int
-- output a := x * 10
-- output b @1kHz := x.aggregate(over: 0.003s, using: sum)

-- sliding window
-- b -> period = 0.001s, over = 0.003s, buckets = 4, bucket size = 0.001s 

-- Note:
-- In RTLola sliding-window aggregation the span of window is inlclusive in the right but exclusive to the left
-- i.e in the output b above, data at the exact time 0.001s won't be included in the aggregation at time 0.003s
-- To deal with this we need one more bucket such that this extreme data can be put into it
-- Also we must not mix this extreme data into the bucket aggregation
-- therefore, when we have both new data and time to slide the window, first we must update the window and then slide it


newtype DataX = DataX Int deriving (Generic, NFDataX, Show)
type HasDataX = (DataX, Bool)
type Inputs = HasDataX

newtype PacingA = PacingA Bool deriving (Generic, NFDataX)
newtype PacingB = PacingBC Bool deriving (Generic, NFDataX) -- every 0.001s

newtype SlideB = SlideBC Bool deriving (Generic, NFDataX) -- every 0.001s

aDflt = (coerce @Int @DataX 0)
bDflt = (coerce @Int @DataX 0)
type BucketsWinB = 4

type Outputs = (HasDataX, HasDataX)


---------------------------------------------------------------
-- Differences to the old architecture
-- 1. Old architecture's hlc alternatively evaluates event-based and periodic pacing, I am doing them at the same time
-- 2. I am grouping the pacings to reduce registers in the queue
-- 3. I am emiting sliding event to the queue just like pacing
-- 4. I have merged the 2 state machines into 1 in LLC
-- 5. Old architecture also works with offline monitor -- this doesn't


---------------------------------------------------------------
-- Total odering of events maintained in the queue

type Slides =  SlideB
type Pacings = (PacingA, PacingB)
type Event = (Inputs, Slides, Pacings)
-- existing architecture keeps event-based and periodic events separately -> alernate one after another

nullEvent :: Event
nullEvent = ((coerce @Int @DataX 0, False), coerce False, (coerce False, coerce False))
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



hlc :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom ((Bool, Event), Int)
hlc inputs = bundle (out, debugSignals)
    where 
        out = bundle (newEvent, event)
        newEvent = newX .||. (coerce slideB) .||. (coerce pacingA) .||. (coerce pacingB)
        event = bundle (inputs, slideB, pacings)
        pacings = bundle (pacingA, pacingB)

        slideB = coerce <$> timer1Over

        (_, newX) = unbundle inputs
        pacingA = coerce <$> newX
        pacingB = coerce <$> timer1Over

        timer1Over = timer1 .>=. period1Ns
        timer1 = timer timer1Over
        timer2Over = timer2 .>=. period2Ns
        timer2 = timer timer2Over
        period1Ns = 1000000 -- 0.001s in ns
        period2Ns = 2000000 -- 0.002s in ns

        timer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int
        timer reset = register 0 (mux reset (pure deltaTime) nextTime)
            where 
                nextTime = timer reset + pure deltaTime
                deltaTime = systemClockPeriodNs

        debugSignals = timer1


---------------------------------------------------------------

data State = StatePop | StateRead |  StateEvalLayer1 | StateEvalLayer2 | StateOutput
    deriving (Generic, Eq, NFDataX)

llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom (Bool, Outputs, (State, Vec BucketsWinB DataX, DataX, PacingA, PacingB, SlideB))
llc event = bundle (toPop, outputs, debugSignals)
    where 
        state = register StatePop nextStateSignal

        -- state: pop
        toPop = state .==. (pure StatePop)
        (isValidEvent, poppedEvent) = unbundle event
        eventInfo = register nullEvent (mux isValidEvent poppedEvent eventInfo)
        (hasDataX, slides, pacings) = unbundle eventInfo
        slideB = slides
        (pacingA, pacingB) = unbundle pacings
        (x, newX) = unbundle hasDataX

        -- state: layer 1
        evalA = evaluateA (state .==. (pure StateEvalLayer1) .&&. (coerce pacingA)) x
        swB = slidingWinB (state .==. (pure StateEvalLayer1)) (coerce slideB) hasDataX

        -- state: layer 2
        evalB = evaluateB (state .==. (pure StateEvalLayer2) .&&. (coerce pacingB)) swB

        -- state: output
        outAktvA = state .==. (pure StateOutput) .&&. (coerce pacingA)
        outAktvB = state .==. (pure StateOutput) .&&. (coerce pacingB)

        outputs = bundle (outputA, outputB)
        outputA = bundle (evalA, outAktvA)
        outputB = bundle (evalB, outAktvB)

        -- state transition
        nextStateSignal = nextState <$> state <*> isValidEvent
        
        nextState :: State -> Bool -> State
        nextState curState validEvent = case curState of
            StatePop -> StateRead
            StateRead -> if validEvent then StateEvalLayer1 else StatePop 
            StateEvalLayer1 -> StateEvalLayer2
            StateEvalLayer2 -> StateOutput
            StateOutput -> StatePop

        debugSignals = bundle (state, swB, x, pacingA, pacingB, slideB)



evaluateA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom DataX
evaluateA en x = operate <$> x 
    where 
        operate :: DataX -> DataX
        operate d = coerce @Int @DataX out
            where out = (coerce @DataX @Int d) * 10

evaluateB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Vec BucketsWinB DataX) -> Signal dom DataX
evaluateB en win = out
    where 
        out = register bDflt (mux en newOut out)
        newOut = merge <$> win

        merge :: Vec BucketsWinB DataX -> DataX
        merge win = fold bBucketFx win


bBucketFx :: DataX -> DataX -> DataX
bBucketFx accum new = out
    where 
        out = coerce @Int @DataX (d1 + d2)
        d1 = coerce @DataX @Int accum
        d2 = coerce @DataX @Int new

slidingWinB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom HasDataX -> Signal dom (Vec BucketsWinB DataX)
slidingWinB en slide hasX = window
    where 
        window = register winDflt (mux en nextWindowSignal window)
        winDflt = repeat bDflt :: Vec BucketsWinB DataX
        nextWindowSignal = nextWindow <$> window <*> slide <*> hasX

        nextWindow :: Vec BucketsWinB DataX -> Bool -> HasDataX -> Vec BucketsWinB DataX
        nextWindow win toSlide inpt = out
            where 
                (x, newX) = inpt
                out = case (toSlide, newX) of 
                    (False, False) -> win
                    (False, True) -> updatedWin
                    (True, False) -> win <<+ bDflt
                    -- update before slide -> existing semantics in RTLola
                    (True, True) -> updatedWin <<+ bDflt
                updatedWin = replace lastIndx (bBucketFx (last win) x) win
                lastIndx = length win - 1
                updated = replace lastIndx (bBucketFx (last win) x) win


---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Outputs, (Int, State, Vec BucketsWinB DataX, Bool, Bool, Bool, Bool, DataX, PacingA, PacingB, SlideB))
monitor inputs = bundle (outputs, debugSignals)
    where 
        (qPushValid, qPopValid, qPopData) = unbundle (queue (bundle (qPush, qPop, qInptData)))

        (eventInfo, hlcDebugInfo) = unbundle (hlc inputs)
        (newEvent, event) = unbundle eventInfo
        qPush = newEvent
        qInptData = event

        (toPop, outputs, llcDebugInfo) = unbundle (llc (bundle (qPopValid, qPopData)))
        qPop = toPop

        -- debug signals
        debugSignals = bundle (hlcTimer, llcState, swB, qPush, qPop, qPushValid, qPopValid, x, pacingA, pacingB, slideB)
        hlcTimer = hlcDebugInfo
        (llcState, swB, x, pacingA, pacingB, slideB) = unbundle llcDebugInfo


---------------------------------------------------------------


topEntity :: Clock MyDomain -> Reset MyDomain -> Enable MyDomain -> 
    Signal MyDomain Inputs -> Signal MyDomain (Outputs, (Int, State, Vec BucketsWinB DataX, Bool, Bool, Bool, Bool, DataX, PacingA, PacingB, SlideB))
topEntity clk rst en input0 = exposeClockResetEnable (monitor input0) clk rst en

