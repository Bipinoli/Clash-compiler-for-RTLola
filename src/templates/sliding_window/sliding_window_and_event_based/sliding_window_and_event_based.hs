{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module SlidingWindow where

import Clash.Prelude
import Data.Coerce

---------------------------------------------------------------

-- specification:

-- input x : Int
-- output a := x * 10
-- output b @1kHz := x.aggregate(over: 0.003s, using: sum)
-- output c @1kHz := x.aggregate(over: 0.004s, using: count)
-- output d @0.5kHz := x.aggregate(over: 0.004s, using: count)

-- sliding window
-- b -> period = 0.001s, over = 0.003s, buckets = 3, bucket size = 0.001s 
-- c -> period = 0.001s, over = 0.004s, buckets = 4, bucket size = 0.001s 
-- d -> period = 0.002s, over = 0.004s, buckets = 2, bucket size = 0.002s 

newtype DataX = DataX Int deriving (Generic, NFDataX)
type HasDataX = (DataX, Bool)

newtype PacingBC = PacingBC Bool deriving (Generic, NFDataX) -- every 0.001s
newtype PacingD = PacingD Bool deriving (Generic, NFDataX) -- every 0.002s

newtype SlideBC = SlideBC Bool deriving (Generic, NFDataX) -- every 0.001s
newtype SlideD = SlideD Bool deriving (Generic, NFDataX) -- every 0.002s
aDflt = (coerce @Int @DataX 0)
bDflt = (coerce @Int @DataX 0)
type DDflt = 0
type BucketsWinB = 3
type BucketsWinC = 4
type BucketsWinD = 2
type BucketBDflt = 0
type BucketCDflt = 0
type BucketDDflt = 0


---------------------------------------------------------------
-- Differences to the old architecture
-- 1. old architecture's hlc alternatively evaluates event-based and periodic pacing, I am doing them at the same time
-- 2. I am grouping the pacings to reduce registers in the queue
-- 3. I am emiting sliding event to the queue just like pacing
-- 4. I have merged the 2 state machines into 1 in LLC


---------------------------------------------------------------
-- Total odering of events maintained in the queue

type Inputs = HasDataX
type Slides = (SlideBC, SlideD)
type Pacings = (PacingBC, PacingD)
type Event = (Inputs, Slides, Pacings)
-- existing architecture keeps event-based and periodic events separately -> alernate one after another

nullEvent :: Event
nullEvent = ((coerce @Int @DataX 0, False), (coerce False, coerce False), (coerce False, coerce False))
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



hlc :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Event, Int)
hlc inputs = bundle (bundle (inputs, slides, pacings), timer1)
    where 
        slides = bundle (coerce slideBC, coerce slideD)
        pacings = bundle (coerce pacingBC, coerce pacingD)

        slideBC = timer1Over
        slideD = timer2Over
        pacingBC = timer1Over
        pacingD = timer2Over

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



---------------------------------------------------------------

type Outputs = (HasDataX, HasDataX)


data State = StatePop | StateReadEvent | StateEvalLayer1 | StateEvalLayer2 | StateOutput
    deriving (Generic, Eq, NFDataX)

llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom (Bool)
llc event = bundle (toPop)
    where 
        state = register StatePop nextStateSignal
        toPop = state .==. (pure StatePop)
        (isValidEvent, poppedEvent) = unbundle event
 
        eventInfo = register nullEvent (mux (state .==. pure StateReadEvent) poppedEvent eventInfo)

        -- TODO:
        -- state 1 -> eval a & sliding window b
        -- state 2 -> eval b
        -- state 3 -> outptu


        nextStateSignal = nextState <$> state <*> isValidEvent
        
        nextState :: State -> Bool -> State
        nextState curState validEvent = case curState of
            StatePop -> if validEvent then StateReadEvent else StatePop 
            StateReadEvent -> StateEvalLayer1
            StateEvalLayer1 -> StateEvalLayer2
            StateEvalLayer2 -> StatePop



evaluateA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom DataX
evaluateA en x = out 
    where 
        out = register aDflt (mux en newOut out)
        newOut = coerce @Int @DataX <$> (old * new)
        old = coerce @DataX @Int <$> out
        new = coerce @DataX @Int <$> x


slidingWinB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom HasDataX -> Signal dom (Vec BucketsWinB DataX)
slidingWinB en slide hasX = window
    where 
        window = register winDflt (mux en nextWindowSignal window)
        winDflt = repeat bDflt :: Vec BucketsWinB DataX
        nextWindowSignal = nextWindow <$> window <*> slide <*> hasX

        nextWindow :: Vec BucketsWinB DataX -> Bool -> HasDataX -> Vec BucketsWinB DataX
        nextWindow win toSlide inpt = out
            where 
                out = if not toSlide then 
                        if not newX then win
                        else replace (length win - 1) (bBucketFx (last win) x) win
                      else 
                        if not newX then win <<+ bDflt
                        else replace (length win - 1) (bBucketFx (last win) x) (win <<+ bDflt)
                (x, newX) = inpt

bBucketFx :: DataX -> DataX -> DataX
bBucketFx accum new = out
    where 
        out = coerce @Int @DataX (d1 + d2)
        d1 = coerce @DataX @Int accum
        d2 = coerce @DataX @Int accum




---------------------------------------------------------------

-- monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom Event
-- monitor inputs = hlc inputs


---------------------------------------------------------------


topEntity :: Clock MyDomain -> Reset MyDomain -> Enable MyDomain -> 
    Signal MyDomain Inputs -> Signal MyDomain (Event, Int)
topEntity clk rst en input0 = exposeClockResetEnable (hlc input0) clk rst en

