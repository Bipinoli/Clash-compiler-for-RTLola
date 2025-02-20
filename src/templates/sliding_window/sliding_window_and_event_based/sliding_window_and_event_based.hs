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
newtype PacingBC = PacingBC Bool deriving (Generic, NFDataX) -- every 0.001s
newtype PacingD = PacingD Bool deriving (Generic, NFDataX) -- every 0.002s

newtype SlideBC = SlideBC Bool deriving (Generic, NFDataX) -- every 0.001s
newtype SlideD = SlideD Bool deriving (Generic, NFDataX) -- every 0.002s


---------------------------------------------------------------
-- Total odering of events maintained in the queue

type Inputs = (DataX, Bool)
type Slides = (SlideBC, SlideD)
type Pacings = (PacingBC, PacingD)
type Event = (Inputs, Slides, Pacings)

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

-- hlc :: HiddenClockResetEnable dom => Int -



---------------------------------------------------------------

-- llc 

---------------------------------------------------------------


-- Create a clock domain with 2 microseconds period (500 kHz) to match the testbench
createDomain vSystem{vName="MyDomain", vPeriod=2000} -- period in nanoseconds

-- systemClockPeriodNs :: Int
-- systemClockPeriodNs = fromInteger (snatToInteger $ clockPeriod @MyDomain)


-- generateClock :: HiddenClockResetEnable dom => Int -> Signal dom (Bool, Int)
-- generateClock desiredPeriodInNs = register initClk (mux (countSignal .<. halfToCount) hiClk loClk)
--   where
--     initClk = (False, 0)
--     hiClk = bundle (pure True, countSignal)
--     loClk = bundle (pure False, countSignal)

--     countSignal = counter toCount
--     toCount = desiredPeriodInNs `div` systemClockPeriodNs
--     halfToCount = pure $ toCount `div` 2

--     counter :: HiddenClockResetEnable dom => Int -> Signal dom Int
--     counter toCount = register 0 (mux (prevVal .<. pure (toCount - 1)) (prevVal + 1) 0)
--         where prevVal = counter toCount

-- clockDivider :: HiddenClockResetEnable dom => Int -> Signal dom (Bool, Int)
-- clockDivider factor = generateClock $ systemClockPeriodNs * factor


-- timer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int
-- timer reset = register deltaTime (mux reset (pure deltaTime) nextTime)
--     where 
--         nextTime = timer reset + pure deltaTime
--         deltaTime = systemClockPeriodNs



-- ---------------------------------------------------------------

-- hotPotato :: HiddenClockResetEnable dom => Int -> Signal dom Int
-- hotPotato n = let s = register 0 (mux (s .==. pure (n-1)) 0 (s + 1)) in s




-- ---------------------------------------------------------------

-- periodBns = 1000000 :: Signal dom Int -- 1kHz in ns
-- bucketBSpanNs = 1000000 -- 0.001s in nanoseconds
-- type BBucketCnt = 3 -- total aggregate = 3 * 0.001 = 0.003s

-- ---------------------------------------------------------------

-- monitor :: HiddenClockResetEnable dom => Signal dom (Int, Bool) -> Signal dom ((Bool, Bool, Int, Bool, Int, Int, Bool, Bool, Int, Int, Vec BBucketCnt Int, Bool), ((Int, Bool), (Int, Bool)))
-- monitor input0 = bundle (debugSignals, outputs)
--     where 
--         -- Note:
--         -- As the input will only be picked on the rising edge of the clock
--         -- It is assumed that the pushed input is sustained until the rising edge
--         inputBuffer = queue (bundle (qPush, qPop, qData))
--         (evalDebugSignals, qPop, outputs) = unbundle (evaluator inputBuffer) 
--         (x, qPush) = unbundle input0
--         qData = input0

--         -- extra debug signals
--         debugSignals = bundle (qPush, qPop, x, qPopValid, qOutX, qWait, enA, enB, stage, timerB, winX, slideB)
--         (qPushValid, qPopValid, qOut, qWait) = unbundle inputBuffer
--         (qOutX, _) = unbundle qOut
--         (enA, enB, stage, timerB, winX, slideB) = unbundle evalDebugSignals




-- ---------------------------------------------------------------


-- pacer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom QOutput -> Signal dom (Int, (Bool, Bool), (Int, Bool, Int), (Bool, Bool), Bool)
-- pacer en inputBuffer = bundle (debugSignals, status, sustainedInput, enables, slides)
--     where 
--         -- inputData & enables must be sustained until next pacer phase
--         -- status must not be sustained
--         status = bundle (toPop, pacerStable) 
--         enables = bundle (enA, enB)
--         slides = slideB

--         toPop = mux (en .&&. (stage .==. (pure 0))) (pure True) (pure False)
--         pacerStable = mux (en .&&. (stage .==. (pure 1))) (pure True) (pure False)

--         sustainedInput = register (0, False, 0) (mux (en .&&. stage .==. (pure 1)) (bundle (x, newX .&&. qPopValid, waitedX)) sustainedInput)

--         (x, newX) = unbundle xData
--         (_, qPopValid, xData, waitedX) = unbundle inputBuffer

--         enA = register False (mux (en .&&. stage .==. (pure 1)) newEnA enA)
--         enB = register False (mux (en .&&. stage .==. (pure 1)) newEnB enB)
--         slideB = register False (mux (en .&&. stage .==. (pure 1)) newSlideB slideB)

--         newEnA = qPopValid .&&. newX
--         newEnB = timerB .>=. periodBns
--         newSlideB = newEnB

--         timerB = timer resetBTimer
--         resetBTimer = en .&&. stage .==. (pure 1) .&&. timerB .>=. periodBns

--         stage = register 0 (nextStage <$> en <*> stage)

--         -- stage 0 -> inactive / active with queue pop
--         -- stage 1 -> active & provide proper enable signals as output
--         nextStage :: Bool -> Int -> Int
--         nextStage enb stg = nxtStg
--             where 
--                 nxtStg = if not enb || stg == 1 then 0 else 1

--         -- debug signals
--         debugSignals = timerB


-- ---------------------------------------------------------------


-- evaluator :: HiddenClockResetEnable dom => Signal dom QOutput -> Signal dom ((Bool, Bool, Int, Int, Vec BBucketCnt Int, Bool), Bool, ((Int, Bool), (Int, Bool)))
-- evaluator inputBuffer = bundle (debugSignals, toPop, outputs)
--     where 
--         -- stage 0 - 1 -> pacing stage
--         -- stage 2 -> eval a & winX4B -- different than the concept of layers provided by RTLola
--         -- stage 3 -> eval b
--         -- stage 4 -> output
--         -- Note: 
--         -- Even though a & b are in the same RTLola layers 
--         -- due to b depending on the sliding window, b must be evaluated 1 cycle later
--         -- thus making the b effectively in a different layer
--         -- i.e with sliding window => effective layer = layer + 1
--         stage = hotPotato 5

--         outputs = bundle (outputA, outputB)
--         outputA = bundle (outA, aktvA)
--         outputB = bundle (outB, aktvB)

--         (pacingDebugSignals, pacingStatus, inputData, enables, slides) = unbundle (pacer (stage .==. (pure 0) .||. stage .==. (pure 1)) inputBuffer)
--         (toPop, _) = unbundle pacingStatus

--         outA = evaluateA (stage .==. (pure 2) .&&. enA) x
--         -- window must be evaluated irrespective of pacing
--         winX4B = slidingWinX4B (stage .==. (pure 2)) slideB inputData

--         outB = evaluateB (stage .==. (pure 3) .&&. enB) winX4B

--         aktvA = (stage .==. (pure 4)) .&&. enA
--         aktvB = (stage .==. (pure 4)) .&&. enB

--         (x, newX, waitedX) = unbundle inputData
--         (enA, enB) = unbundle enables
--         slideB = slides

--         --- debug signals
--         debugSignals = bundle (enA, enB, stage, timerB, winX4B, slideB)
--         timerB = pacingDebugSignals




-- evaluateA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom Int
-- evaluateA en x = out 
--     where out = register 0 (mux en (x * 10) out)


-- -- evaluation freq of a = 40kHz = 0.000025s
-- -- aggregate over = 0.000050s
-- -- buckets required = 2
-- -- bucket span = 0.000025s
-- slidingWinX4B :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom (Int, Bool, Int) -> Signal dom (Vec BBucketCnt Int)
-- slidingWinX4B en slide timedInput = window
--     where 
--         window = register winDflt (mux en nextWindowSignal window)
--         winDflt = repeat bktDflt :: Vec BBucketCnt Int
--         bktDflt = 0 :: Int
--         nextWindowSignal = nextWindow <$> window <*> slide <*> timedInput

--         nextWindow :: Vec BBucketCnt Int -> Bool -> (Int, Bool, Int) -> Vec BBucketCnt Int
--         nextWindow win toSlide inpt = out
--             where 
--                 out = if not toSlide then 
--                         if not newX || bucketsBefore >= length win then win
--                         else updateBucket win (length win - 1 - bucketsBefore) x
--                       else 
--                         if not newX || bucketsBefore >= length win - 1 then (win <<+ bktDflt)
--                         else updateBucket (win <<+ bktDflt) (length win - 1 - bucketsBefore) x

--                 bucketsBefore = (cyclesBefore * systemClockPeriodNs) `div` bucketBSpanNs 
--                 (x, newX, cyclesBefore) = inpt

--         updateBucket :: Vec BBucketCnt Int -> Int -> Int -> Vec BBucketCnt Int
--         updateBucket win index value = map (\(indx, v) -> if indx == index then bBucketFx v value else v) (zip indices win)
--             where indices = iterateI (+1) 0 :: Vec BBucketCnt Int

-- bBucketFx :: Int -> Int -> Int
-- bBucketFx accum new = accum + new

-- evaluateB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Vec BBucketCnt Int) -> Signal dom Int
-- evaluateB en winX = register 0 (mux en newVal oldVal)
--     where 
--         oldVal = evaluateB en winX
--         newVal = (fold bBucketFx) <$> winX



-- ---------------------------------------------------------------


-- topEntity :: Clock MyDomain -> Reset MyDomain -> Enable MyDomain -> 
--     Signal MyDomain (Int, Bool) -> Signal MyDomain ((Bool, Bool, Int, Bool, Int, Int, Bool, Bool, Int, Int, Vec BBucketCnt Int, Bool), ((Int, Bool), (Int, Bool)))
-- topEntity clk rst en input0 = exposeClockResetEnable (monitor input0) clk rst en