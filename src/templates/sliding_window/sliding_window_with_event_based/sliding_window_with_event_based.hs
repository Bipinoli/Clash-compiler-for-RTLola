module SlidingWindowWithEventBased where

import Clash.Prelude


---------------------------------------------------------------

-- Create a clock domain with 2 microseconds period (500 kHz) to match the testbench
createDomain vSystem{vName="MyDomain", vPeriod=2000} -- period in nanoseconds

systemClockPeriodNs :: Int
systemClockPeriodNs = fromInteger (snatToInteger $ clockPeriod @MyDomain)


generateClock :: HiddenClockResetEnable dom => Int -> Signal dom (Bool, Int)
generateClock desiredPeriodInNs = register initClk (mux (countSignal .<. halfToCount) hiClk loClk)
  where
    initClk = (False, 0)
    hiClk = bundle (pure True, countSignal)
    loClk = bundle (pure False, countSignal)

    countSignal = counter toCount
    toCount = desiredPeriodInNs `div` systemClockPeriodNs
    halfToCount = pure $ toCount `div` 2

    counter :: HiddenClockResetEnable dom => Int -> Signal dom Int
    counter toCount = register 0 (mux (prevVal .<. pure (toCount - 1)) (prevVal + 1) 0)
        where prevVal = counter toCount

clockDivider :: HiddenClockResetEnable dom => Int -> Signal dom (Bool, Int)
clockDivider factor = generateClock $ systemClockPeriodNs * factor


timer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int
timer reset = register deltaTime (mux reset (pure deltaTime) nextTime)
    where 
        nextTime = timer reset + pure deltaTime
        deltaTime = systemClockPeriodNs



---------------------------------------------------------------

hotPotato :: HiddenClockResetEnable dom => Int -> Signal dom Int
hotPotato n = let s = register 0 (mux (s .==. pure (n-1)) 0 (s + 1)) in s


---------------------------------------------------------------

type QData = (Int, Bool)
qNullData = (0, False) :: QData

type QMemSize = 5

type QMem = Vec QMemSize QData
type QWait = Vec QMemSize Int
type QCursor = Int
type QPush = Bool
type QPop = Bool
type QPushValid = Bool
type QPopValid = Bool

type QState = (QMem, QMem, QCursor)
type QInput = (QPush, QPop, QData)
type QOutput = (QPushValid, QPopValid, QData, Int)

-- Note: assumed that queue never overflows
queue :: HiddenClockResetEnable dom => Signal dom QInput -> Signal dom QOutput
queue input = output
    where 
        -- keeping in registers to avoid any combinational output
        output = bundle (pushValid, popValid, outData, waited)
        state = bundle (buffer, wait, cursor)
        buffer = register (repeat qNullData :: QMem) nextBufferSignal
        wait = register (repeat 0 :: QWait) nextWaitSignal
        cursor = register 0 nextCursorSignal
        (outData, waited) = unbundle (register (qNullData, -1) nextOutDataSignal)
        pushValid = register False nextPushValidSignal
        popValid = register False nextPopValidSignal

        -- Signal is an applicative functor
        -- <$> = alias for fmap - wraps normal function into a function on Signal
        -- <*> = apply applicative - it applies signal of function to a signal of a value

        nextBufferSignal = nextBuffer <$> buffer <*> bundle (input, cursor)
        nextWaitSignal = nextWait <$> wait <*> bundle (input, cursor)
        nextCursorSignal = nextCursor <$> cursor <*> bundle (input, buffer)
        nextOutDataSignal = nextOutData <$> bundle (input, cursor, buffer, wait)
        nextPushValidSignal = nextPushValid <$> bundle (input, cursor, buffer)
        nextPopValidSignal = nextPopValid <$> bundle (input, cursor)
        
        nextBuffer :: QMem -> (QInput, QCursor) -> QMem
        nextBuffer buf ((push, pop, qData), cur) = out
            where 
                out = case (push, pop) of
                    (True, _) -> if cur /= length buf then qData +>> buf else buf
                    (_, _) -> buf

        nextWait :: QWait -> (QInput, QCursor) -> QWait
        nextWait curWait curInptCursor = map updateWait (prepForUpdate curWait curInptCursor)
            where
                updateWait :: (Int, Int, Int, QCursor, QPush, QPop) -> Int
                updateWait (valToLeft, val, index, cur, push, pop) = out
                    where 
                        out = case (push, pop) of
                            (False, False) -> if index < cur then val + 1 else -1
                            (True, False) -> if index == 0 then 0 
                                            else if index <= cur then valToLeft + 1 else -1
                            (False, True) -> if index < (cur - 1) then val + 1 else -1
                            (True, True) -> if index == 0 then 0
                                            else if index < cur then valToLeft + 1 else -1

                prepForUpdate :: QWait -> (QInput, QCursor) -> Vec QMemSize (Int, Int, Int, QCursor, QPush, QPop)
                prepForUpdate curWait ((push, pop, _), cur) = out
                    where 
                        indices = iterateI (+1) 0 :: Vec QMemSize Int
                        shiftedRight = (0 :> Nil) ++ init curWait
                        out = zipWith (\(psh, pp, c) (i, valToLeft, val) -> (valToLeft, val, i, c, psh, pp))
                                (repeat (push, pop, cur))
                                (zip3 indices shiftedRight curWait)


        nextCursor :: QCursor -> (QInput, QMem) -> QCursor
        nextCursor cur ((push, pop, _), buf) = out
            where 
                out = case (push, pop) of
                    (True, False) -> if cur /= length buf then cur + 1 else cur
                    (False, True) -> if cur /= 0 then cur - 1 else 0
                    (True, True) -> if cur == 0 then cur + 1 else cur 
                    (_, _) -> cur

        nextOutData :: (QInput, QCursor, QMem, QWait) -> (QData, Int)
        nextOutData ((push, pop, _), cur, buf, _wait) = out
            where 
                out = case (push, pop) of
                    (_, True) -> if cur == 0 then (qNullData, -1) else (buf !! (cur - 1), (_wait !! (cur - 1)) + 1)
                    (_, _) -> (qNullData, -1)

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


monitor :: HiddenClockResetEnable dom => Signal dom (Int, Bool) -> Signal dom ((Bool, Bool, Int, Bool, Bool, Int, Int, Bool, Bool, Int, Int, Vec 2 Int), ((Int, Bool), (Int, Bool)))
monitor input0 = bundle (debugSignals, outputs)
    where 
        inputBuffer = queue (bundle (qPush, qPop, qData))
        (evalDebugSignals, qPop, outputs) = unbundle (evaluator inputBuffer) 
        (x, qPush) = unbundle input0
        qData = input0

        -- extra debug signals
        debugSignals = bundle (qPush, qPop, x, qPushValid, qPopValid, qOutX, qWait, enA, enB, stage, timerB, winX)
        (qPushValid, qPopValid, qOut, qWait) = unbundle inputBuffer
        (qOutX, _) = unbundle qOut
        (enA, enB, stage, timerB, winX) = unbundle evalDebugSignals




---------------------------------------------------------------


pacer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom QOutput -> Signal dom (Int, (Bool, Bool), (Int, Bool, Int), (Bool, Bool))
pacer en inputBuffer = bundle (debugSignals, status, inputData, enables)
    where 
        status = bundle (toPop, pacerStable) 
        inputData = bundle (x, newX .&&. qPopValid, waitedX)
        enables = bundle (enA, enB)

        toPop = mux (en .&&. (stage .==. (pure 0))) (pure True) (pure False)
        pacerStable = mux (en .&&. (stage .==. (pure 1))) (pure True) (pure False)

        (x, newX) = unbundle xData
        (_, qPopValid, xData, waitedX) = unbundle inputBuffer

        enA = register False (mux (en .&&. stage .==. (pure 1)) newEnA enA)
        enB = register False (mux (en .&&. stage .==. (pure 1)) newEnB enB)

        newEnA = qPopValid .&&. newX
        newEnB = timerB .>=. periodBns

        timerB = timer resetBTimer
        resetBTimer = en .&&. stage .==. (pure 1) .&&. timerB .>=. periodBns
        periodBns = 1000000 :: Signal dom Int -- 1kHz in ns

        stage = register 0 (nextStage <$> en <*> stage)

        -- stage 0 -> inactive / active with queue pop
        -- stage 1 -> active & provide proper enable signals as output
        nextStage :: Bool -> Int -> Int
        nextStage enb stg = nxtStg
            where 
                nxtStg = if not enb || stg == 1 then 0 else 1

        -- debug signals
        debugSignals = timerB


---------------------------------------------------------------


evaluator :: HiddenClockResetEnable dom => Signal dom QOutput -> Signal dom ((Bool, Bool, Int, Int, Vec 2 Int), Bool, ((Int, Bool), (Int, Bool)))
evaluator inputBuffer = bundle (debugSignals, toPop, outputs)
    where 
        -- stage 0 - 1 -> pacing stage
        -- stage 2 -> eval a & winX4B -- different than the concept of layers provided by RTLola
        -- stage 3 -> eval b
        -- stage 4 -> output
        stage = hotPotato 5

        outputs = bundle (outputA, outputB)
        outputA = bundle (outA, aktvA)
        outputB = bundle (outB, aktvB)

        (pacingDebugSignals, pacingStatus, inputData, enables) = unbundle (pacer (stage .==. (pure 0) .||. stage .==. (pure 1)) inputBuffer)
        (toPop, _) = unbundle pacingStatus

        outA = evaluateA (stage .==. (pure 2) .&&. enA) x
        winX4B = windowXForB (stage .==. (pure 2)) inputData

        outB = evaluateB (stage .==. (pure 3) .&&. enB) winX4B

        aktvA = (stage .==. (pure 4)) .&&. enA
        aktvB = (stage .==. (pure 4)) .&&. enB

        (x, newX, waitedX) = unbundle inputData
        (enA, enB) = unbundle enables

        --- debug signals
        debugSignals = bundle (enA, enB, stage, timerB, winX4B)
        timerB = pacingDebugSignals




evaluateA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom Int
evaluateA en x = out 
    where out = register 0 (mux en (x * 10) out)


-- evaluation freq of a = 1kHz = 0.001s
-- aggregate over = 0.002s
-- buckets required = 2
-- bucket span = 0.001s
windowXForB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Int, Bool, Int) -> Signal dom (Vec 2 Int)
windowXForB en timedInput = window
    where 
        window = register winDflt (mux en nextWindowSignal window)
        winDflt = repeat 0 :: Vec 2 Int
        nextWindowSignal = nextWindow <$> window <*> timedInput

        nextWindow :: Vec 2 Int -> (Int, Bool, Int) -> Vec 2 Int 
        nextWindow win inpt = out
            where 
                out = if not newX || bucketsBefore >= length win then win
                      else updateBucket win (length win - 1 - bucketsBefore) x
                bucketsBefore = (cyclesBefore * systemClockPeriodNs) `div` bucketSpanNs 
                (x, newX, cyclesBefore) = inpt
                bucketSpanNs = 1_000_000 -- 0.001s in nanoseconds

        updateBucket :: Vec 2 Int -> Int -> Int -> Vec 2 Int
        updateBucket win index value = map (\(indx, v) -> if indx == index then bBucketFx v value else v) (zip indices win)
            where indices = iterateI (+1) 0 :: Vec 2 Int

bBucketFx :: Int -> Int -> Int
bBucketFx accum new = accum + new

evaluateB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Vec 2 Int) -> Signal dom Int
evaluateB en winX = register 0 (mux en newVal oldVal)
    where 
        oldVal = evaluateB en winX
        newVal = (fold bBucketFx) <$> winX



---------------------------------------------------------------


topEntity :: Clock MyDomain -> Reset MyDomain -> Enable MyDomain -> 
    Signal MyDomain (Int, Bool) -> Signal MyDomain ((Bool, Bool, Int, Bool, Bool, Int, Int, Bool, Bool, Int, Int, Vec 2 Int), ((Int, Bool), (Int, Bool)))
topEntity clk rst en input0 = exposeClockResetEnable (monitor input0) clk rst en
