module SlidingWindowDiffFreq where

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

type InputXTyp = Int

type QData = (InputXTyp, Bool)
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
type QOutput = (QPushValid, QPopValid, QData)

-- Note: assumed that queue never overflows
queue :: HiddenClockResetEnable dom => Signal dom QInput -> Signal dom QOutput
queue input = output
    where 
        -- keeping in registers to avoid any combinational output
        output = bundle (pushValid, popValid, outData)
        state = bundle (buffer, wait, cursor)
        buffer = register (repeat qNullData :: QMem) nextBufferSignal
        wait = register (repeat 0 :: QWait) nextWaitSignal
        cursor = register 0 nextCursorSignal
        outData = register qNullData nextOutDataSignal
        pushValid = register False nextPushValidSignal
        popValid = register False nextPopValidSignal

        -- Signal is an applicative functor
        -- <$> = alias for fmap - wraps normal function into a function on Signal
        -- <*> = apply applicative - it applies signal of function to a signal of a value

        nextBufferSignal = nextBuffer <$> buffer <*> bundle (input, cursor)
        nextWaitSignal = nextWait <$> wait <*> bundle (input, cursor)
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

        nextOutData :: (QInput, QCursor, QMem) -> QData
        nextOutData ((push, pop, _), cur, buf) = out
            where 
                out = case (push, pop) of
                    (_, True) -> if cur == 0 then qNullData else buf !! (cur - 1)
                    (_, _) -> qNullData 

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



-- type Data = Int

-- slowClock :: HiddenClockResetEnable dom => Signal dom (Bool, Int)
-- slowClock = clockDivider 5

-- -- We need to sustain control signals and data from HLC until all layers are completely evaluated and outputed
-- -- It talke one cycle for RTL to HLC and we want one cycle in the end to output the evaluated streams
-- -- Hence, the LLC should run (n+2) times faster than HLC, where n = number of evaluation layers
-- -- By sustaining the HLC controls like this we also don't need any buffer or queue to interface them
-- -- Note: due to periodEnable singals in HLC we need one more RTL in HLC so (n+3)

-- -- It is important to note that the input data must be proided by stage = 0 to work with new data on the current LLC loop
-- evaluator :: HiddenClockResetEnable dom => Signal dom (Data, Bool) -> Signal dom ((Bool, Int), (Data, Bool, Bool, Bool, Int, Int, Int), (Int, (Data, Bool), (Data, Bool), (Data, Bool)))
-- evaluator input0 = bundle(hlcClk, controls, outputs)
--     where
--         outputs = llc controls stage
--         controls = hlc hlcClk input0
--         hlcClk = slowClock
--         stage = delay 0 (hotPotato 5)


-- ---------------------------------------------------------------

-- -- since there is one more RTL for periodicEnable in HLC we should wait 1 more cycle until data consumtion in LLC
-- hlc :: HiddenClockResetEnable dom => Signal dom (Bool, Int) -> Signal dom (Data, Bool) -> Signal dom (Data, Bool, Bool, Bool, Int, Int, Int)
-- hlc hlcClk input0 = register (0, False, False, False, 0, 0, 0) (mux en newResult oldResult)
--     where
--         (en, enCnt) = unbundle hlcClk
--         oldResult = bundle (oldA, oldEnB, oldEnC, oldEnD, bTimer, cTimer, dTimer)
--         (oldA, oldEnB, oldEnC, oldEnD, oldBTimer, oldCTimer, oldDTimer) = unbundle (hlc hlcClk input0)
--         newResult = bundle (a, enB, enC, enD, bTimer, cTimer, dTimer)
--         (a, aktv_x) = unbundle input0

--         (enB, bTimer) = unbundle (periodicEnable hlcClk bPeriodNs)
--         (enC, cTimer) = unbundle (periodicEnable hlcClk cPeriodNs)
--         (enD, dTimer) = unbundle (periodicEnable hlcClk dPeriodNs)

--         bPeriodNs = 100000 -- 10Hz in ns
--         cPeriodNs = 200000 -- 5Hz in ns
--         dPeriodNs = 400000 -- 2.5Hz in ns


-- -- NOTE:
-- -- Once output enable is high, it must sustain until the next cycle
-- -- Timer should immediately restart without being sustained
-- periodicEnable :: HiddenClockResetEnable dom => Signal dom (Bool, Int) -> Signal dom Int -> Signal dom (Bool, Int)
-- periodicEnable hlcClk period = register initState (mux (en .&&. (cnt .==. pure 0)) nextState curState) -- cnt .==.0 to sustain for a cycle
--         where
--             (en, cnt) = unbundle hlcClk 
--             initState = (False, 0)
--             nextState = bundle (nextEn, nextTime)
--             curState = bundle (curEn, nextTime)

--             (curEn, curTime) = unbundle (periodicEnable hlcClk period)
--             nextEn = mux (nextTime .>=. period) (pure True) (pure False)
--             reset = mux (en .&&. (nextTime .>=. period)) (pure True) (pure False)
--             nextTime = timer reset


-- ---------------------------------------------------------------


-- llc :: HiddenClockResetEnable dom => Signal dom (Data, Bool, Bool, Bool, Int, Int, Int) -> Signal dom Data -> Signal dom (Int, (Data, Bool), (Data, Bool), (Data, Bool))
-- llc controls stage = bundle (stage, resultB, resultC, resultD)
--     where 
--         resultB = bundle (outB, aktvB)
--         resultC = bundle (outC, aktvC)
--         resultD = bundle (outD, aktvD)
--         -- 2 cycles for RTL in HLC i.e stage = 0 & stage = 1
--         -- b & c are in the evaluation layer 1 -> stage = 2
--         outB = evaluateB ((stage .==. pure 2) .&&. enB) a
--         -- d is in evaluation layer 2 -> stage = 3
--         outD = evaluateD ((stage .==. pure 3) .&&. enD) (bundle (outB, outC))
--         -- output stage -> stage = 4
--         (aktvB, aktvC, aktvD) = unbundle $ mux (stage .==. pure 4) (bundle (enB, enC, enD)) (bundle (pure False, pure False, pure False)) 
--         (a, enB, enC, enD, _, _, _) = unbundle controls


-- TODO: handle the time difference due to data transfer properly

-- evaluation freq of a = 1kHz = 0.001s
-- aggregate over = 0.002s
-- buckets required = 2
-- bucket span = 0.001s
windowXForA :: HiddenClockResetEnable dom => Signal dom (InputXTyp, Bool, Int) -> Signal dom (Vec 2 Int)
windowXForA timedInput = window
    where 
        window = register winDflt nextWindowSignal
        winDflt = repeat 0 :: Vec 2 Int
        nextWindowSignal = nextWindow <$> window <*> timedInput

        nextWindow :: Vec 2 Int -> (InputXTyp, Bool, Int) -> Vec 2 Int 
        nextWindow win inpt = out
            where 
                out = if not newX || bucketsBefore >= length win then win
                      else updateBucket win (length win - 1 - bucketsBefore) x
                bucketsBefore = (cyclesBefore * systemClockPeriodNs) `div` bucketSpanNs 
                (x, newX, cyclesBefore) = inpt
                bucketSpanNs = 1_000_000 -- 0.001s in nanoseconds

        updateBucket :: Vec 2 Int -> Int -> Int -> Vec 2 Int
        updateBucket win index value = map (\(indx, v) -> if indx == index then bucketFx v value else v) (zip indices win)
            where indices = iterateI (+1) 0 :: Vec 2 Int

        bucketFx :: Int -> Int -> Int
        bucketFx accum new = accum + new




-- evaluateA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom Int
-- evaluateA en inpt = register 0 ()


-- ---------------------------------------------------------------


-- topEntity :: Clock MyDomain -> Reset MyDomain -> Enable MyDomain -> 
--     Signal MyDomain (Data, Bool) -> Signal MyDomain ((Bool, Int), (Data, Bool, Bool, Bool, Int, Int, Int), (Int, (Data, Bool), (Data, Bool), (Data, Bool)))
-- topEntity clk rst en input0 = exposeClockResetEnable (evaluator input0) clk rst en
