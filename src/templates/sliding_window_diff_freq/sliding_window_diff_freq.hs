module SlidingWindow where

import Clash.Prelude


---------------------------------------------------------------

-- Create a clock domain with 2 microseconds period (500 kHz) to match the testbench
createDomain vSystem{vName="MyDomain", vPeriod=2000} -- period in nanoseconds

systemClockPeriod :: Int
systemClockPeriod = fromInteger (snatToInteger $ clockPeriod @MyDomain)


generateClock :: HiddenClockResetEnable dom => Int -> Signal dom (Bool, Int)
generateClock desiredPeriodInNs = register initClk (mux (countSignal .<. halfToCount) hiClk loClk)
  where
    initClk = (False, 0)
    hiClk = bundle (pure True, countSignal)
    loClk = bundle (pure False, countSignal)

    countSignal = counter toCount
    toCount = desiredPeriodInNs `div` systemClockPeriod
    halfToCount = pure $ toCount `div` 2

    counter :: HiddenClockResetEnable dom => Int -> Signal dom Int
    counter toCount = register 0 (mux (prevVal .<. pure (toCount - 1)) (prevVal + 1) 0)
        where prevVal = counter toCount

clockDivider :: HiddenClockResetEnable dom => Int -> Signal dom (Bool, Int)
clockDivider factor = generateClock $ systemClockPeriod * factor


timer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int
timer reset = register deltaTime (mux reset (pure deltaTime) nextTime)
    where 
        nextTime = timer reset + pure deltaTime
        deltaTime = systemClockPeriod




---------------------------------------------------------------

hotPotato :: HiddenClockResetEnable dom => Int -> Signal dom Int
hotPotato n = let s = register 0 (mux (s .==. pure (n-1)) 0 (s + 1)) in s


---------------------------------------------------------------

type Data = Int

slowClock :: HiddenClockResetEnable dom => Signal dom (Bool, Int)
slowClock = clockDivider 5

-- We need to sustain control signals and data from HLC until all layers are completely evaluated and outputed
-- It talke one cycle for RTL to HLC and we want one cycle in the end to output the evaluated streams
-- Hence, the LLC should run (n+2) times faster than HLC, where n = number of evaluation layers
-- By sustaining the HLC controls like this we also don't need any buffer or queue to interface them
-- Note: due to periodEnable singals in HLC we need one more RTL in HLC so (n+3)

-- It is important to note that the input data must be proided by stage = 0 to work with new data on the current LLC loop
evaluator :: HiddenClockResetEnable dom => Signal dom (Data, Bool) -> Signal dom ((Bool, Int), (Data, Bool, Bool, Bool, Int, Int, Int), (Int, (Data, Bool), (Data, Bool), (Data, Bool)))
evaluator input0 = bundle(hlcClk, controls, outputs)
    where
        outputs = llc controls stage
        controls = hlc hlcClk input0
        hlcClk = slowClock
        stage = delay 0 (hotPotato 5)


---------------------------------------------------------------

-- since there is one more RTL for periodicEnable in HLC we should wait 1 more cycle until data consumtion in LLC
hlc :: HiddenClockResetEnable dom => Signal dom (Bool, Int) -> Signal dom (Data, Bool) -> Signal dom (Data, Bool, Bool, Bool, Int, Int, Int)
hlc hlcClk input0 = register (0, False, False, False, 0, 0, 0) (mux en newResult oldResult)
    where
        (en, enCnt) = unbundle hlcClk
        oldResult = bundle (oldA, oldEnB, oldEnC, oldEnD, bTimer, cTimer, dTimer)
        (oldA, oldEnB, oldEnC, oldEnD, oldBTimer, oldCTimer, oldDTimer) = unbundle (hlc hlcClk input0)
        newResult = bundle (a, enB, enC, enD, bTimer, cTimer, dTimer)
        (a, aktv_x) = unbundle input0

        (enB, bTimer) = unbundle (periodicEnable hlcClk bPeriodNs)
        (enC, cTimer) = unbundle (periodicEnable hlcClk cPeriodNs)
        (enD, dTimer) = unbundle (periodicEnable hlcClk dPeriodNs)

        bPeriodNs = 100000 -- 10Hz in ns
        cPeriodNs = 200000 -- 5Hz in ns
        dPeriodNs = 400000 -- 2.5Hz in ns


-- NOTE:
-- Once output enable is high, it must sustain until the next cycle
-- Timer should immediately restart without being sustained
periodicEnable :: HiddenClockResetEnable dom => Signal dom (Bool, Int) -> Signal dom Int -> Signal dom (Bool, Int)
periodicEnable hlcClk period = register initState (mux (en .&&. (cnt .==. pure 0)) nextState curState) -- cnt .==.0 to sustain for a cycle
        where
            (en, cnt) = unbundle hlcClk 
            initState = (False, 0)
            nextState = bundle (nextEn, nextTime)
            curState = bundle (curEn, nextTime)

            (curEn, curTime) = unbundle (periodicEnable hlcClk period)
            nextEn = mux (nextTime .>=. period) (pure True) (pure False)
            reset = mux (en .&&. (nextTime .>=. period)) (pure True) (pure False)
            nextTime = timer reset


---------------------------------------------------------------


llc :: HiddenClockResetEnable dom => Signal dom (Data, Bool, Bool, Bool, Int, Int, Int) -> Signal dom Data -> Signal dom (Int, (Data, Bool), (Data, Bool), (Data, Bool))
llc controls stage = bundle (stage, resultB, resultC, resultD)
    where 
        resultB = bundle (outB, aktvB)
        resultC = bundle (outC, aktvC)
        resultD = bundle (outD, aktvD)
        -- 2 cycles for RTL in HLC i.e stage = 0 & stage = 1
        -- b & c are in the evaluation layer 1 -> stage = 2
        outB = evaluateB ((stage .==. pure 2) .&&. enB) a
        outC = evaluateC ((stage .==. pure 2) .&&. enC) a
        -- d is in evaluation layer 2 -> stage = 3
        outD = evaluateD ((stage .==. pure 3) .&&. enD) (bundle (outB, outC))
        -- output stage -> stage = 4
        (aktvB, aktvC, aktvD) = unbundle $ mux (stage .==. pure 4) (bundle (enB, enC, enD)) (bundle (pure False, pure False, pure False)) 
        (a, enB, enC, enD, _, _, _) = unbundle controls


evaluateA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom Int
evaluateA en inpt = register 0 ()

evaluateB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data
evaluateB en inpt = register 10 (mux en a oldVal)
    where 
        oldVal = evaluateB en inpt
        a = inpt

---------------------------------------------------------------


topEntity :: Clock MyDomain -> Reset MyDomain -> Enable MyDomain -> 
    Signal MyDomain (Data, Bool) -> Signal MyDomain ((Bool, Int), (Data, Bool, Bool, Bool, Int, Int, Int), (Int, (Data, Bool), (Data, Bool), (Data, Bool)))
topEntity clk rst en input0 = exposeClockResetEnable (evaluator input0) clk rst en
