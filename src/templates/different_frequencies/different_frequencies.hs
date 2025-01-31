module DifferentFrequencies where

import Clash.Prelude


---------------------------------------------------------------

-- Create a clock domain with 2 microseconds period (500 kHz) to match the testbench
createDomain vSystem{vName="MyDomain", vPeriod=2000} -- period in nanoseconds

systemClockPeriod = snatToInteger $ clockPeriod @MyDomain


generateClock :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
generateClock desiredPeriodInNs = register False (mux (countSignal .<. halfToCount) (pure True) (pure False))
  where
    countSignal = counter toCount
    toCount = desiredPeriodInNs `div` systemClockPeriod
    halfToCount = pure $ toCount `div` 2

    counter :: HiddenClockResetEnable dom => Integer -> Signal dom Integer
    counter toCount = register 0 (mux (prevVal .<. pure (toCount - 1)) (prevVal + 1) 0)
        where prevVal = counter toCount

clockDivider :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
clockDivider factor = generateClock $ systemClockPeriod * factor


timer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int
timer reset = register deltaTime (mux reset (pure deltaTime) nextTime)
    where 
        nextTime = timer reset + pure deltaTime
        deltaTime = fromInteger systemClockPeriod :: Int




---------------------------------------------------------------

hotPotato :: HiddenClockResetEnable dom => Int -> Signal dom Int
hotPotato n = let s = register 0 (mux (s .==. pure (n-1)) 0 (s + 1)) in s


---------------------------------------------------------------

type Data = Int

slowClock :: HiddenClockResetEnable dom => Signal dom Bool
slowClock = clockDivider 5

-- We need to sustain control signals and data from HLC until all layers are completely evaluated and outputed
-- It talke one cycle for RTL to HLC and we want one cycle in the end to output the evaluated streams
-- Hence, the LLC should run (n+2) times faster than HLC, where n = number of evaluation layers
-- By sustaining the HLC controls like this we also don't need any buffer or queue to interface them
-- Note: due to periodEnable singals in HLC we need one more RTL in HLC so (n+3)

-- It is important to note that the input data must be proided by stage = 0 to work with new data on the current LLC loop
evaluator :: HiddenClockResetEnable dom => Signal dom (Data, Bool) -> Signal dom (Bool, (Data, Bool, Bool, Bool, Int, Int, Int, Bool, Bool, Bool), (Int, (Data, Bool), (Data, Bool), (Data, Bool)))
evaluator input0 = bundle(slowClock, controls, outputs)
    where
        outputs = llc controls stage
        controls = hlc slowClock input0
        stage = delay 0 (hotPotato 5)


---------------------------------------------------------------

-- since there is one more RTL for periodicEnable in HLC we should wait 1 more cycle until data consumtion in LLC
hlc :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Data, Bool) -> Signal dom (Data, Bool, Bool, Bool, Int, Int, Int, Bool, Bool, Bool)
hlc en input0 = register (0, False, False, False, 0, 0, 0, False, False, False) (mux en newResult oldResult)
    where
        oldResult = bundle (oldA, oldEnB, oldEnC, oldEnD, bTimer, cTimer, dTimer, bTimerRst, cTimerRst, dTimerRst)
        (oldA, oldEnB, oldEnC, oldEnD, oldBTimer, oldCTimer, oldDTimer, _, _, _) = unbundle (hlc en input0)
        newResult = bundle (a, enB, enC, enD, bTimer, cTimer, dTimer, bTimerRst, cTimerRst, dTimerRst)
        (a, aktv_x) = unbundle input0

        (enB, bTimer ) = unbundle (periodicEnable en bPeriodNs)
        (enC, cTimer) = unbundle (periodicEnable en cPeriodNs)
        (enD, dTimer, dTimerRst) = unbundle (periodicEnable en dPeriodNs)

        bPeriodNs = 100000 -- 10Hz in ns
        cPeriodNs = 200000 -- 5Hz in ns
        dPeriodNs = 400000 -- 2.5Hz in ns


periodicEnable :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom (Bool, Int)
periodicEnable en period = register initState (mux en nextState curState)
        where
            initState = (False, 0)
            nextState = bundle (nextEn, nextTime)
            curState = bundle (curEn, nextTime)

            (curEn, curTime) = unbundle (periodicEnable en period)
            nextEn = mux (curTime .>=. period) (pure True) (pure False)
            reset = mux (en .&&. (curTime .>=. period)) (pure True) (pure False)
            nextTime = timer reset


---------------------------------------------------------------


llc :: HiddenClockResetEnable dom => Signal dom (Data, Bool, Bool, Bool, Int, Int, Int, Bool, Bool, Bool) -> Signal dom Data -> Signal dom (Int, (Data, Bool), (Data, Bool), (Data, Bool))
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
        (a, enB, enC, enD, _, _, _, _, _, _) = unbundle controls


evaluateB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data
evaluateB en inpt = register 10 (mux en a oldVal)
    where 
        oldVal = evaluateB en inpt
        a = inpt

evaluateC :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data
evaluateC en inpt = register 10 (mux en a oldVal)
    where 
        oldVal = evaluateB en inpt
        a = inpt

evaluateD :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Data, Data) -> Signal dom Data
evaluateD en inpt = register 10 (mux en (b + c) oldVal)
    where 
        oldVal = evaluateD en inpt
        (b, c) = unbundle inpt


---------------------------------------------------------------


topEntity :: Clock MyDomain -> Reset MyDomain -> Enable MyDomain -> 
    Signal MyDomain (Data, Bool) -> Signal MyDomain (Bool, (Data, Bool, Bool, Bool, Int, Int, Int, Bool, Bool, Bool), (Int, (Data, Bool), (Data, Bool), (Data, Bool)))
topEntity clk rst en input0 = exposeClockResetEnable (evaluator input0) clk rst en
