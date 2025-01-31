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
timer reset = register 0 (mux reset 0 nextTime)
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

-- It is important to note that the input data must be proided by stage = 0 to work with new data on the current LLC loop
evaluator :: HiddenClockResetEnable dom => Signal dom (Data, Bool) -> Signal dom (Bool, (Data, Bool, Bool, Bool, Int), (Int, (Data, Bool), (Data, Bool), (Data, Bool)))
evaluator input0 = bundle(slowClock, controls, outputs)
    where
        outputs = llc controls stage
        controls = hlc slowClock input0
        stage = delay 0 (hotPotato 4)


hlc :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Data, Bool) -> Signal dom (Data, Bool, Bool, Bool, Int)
hlc en input0 = register (0, False, False, False, 0) (mux en newResult oldResult)
    where
        oldResult = hlc en input0
        newResult = bundle (a, enB, enC, enD, bTimer)
        (a, aktv_x) = unbundle input0

        -- TODO: why are the enable singals not working as expected???
        -- Why does enable signal not sustain until the hlc clock?
        enB = mux (bTimer .>=. bPeriodNs) (pure True) (pure False)
        bTimer = timer bTimerRst
        bTimerRst = mux (bTimer .>=. bPeriodNs) (pure True) (pure False)

        enC = mux (cTimer .>=. cPeriodNs) (pure True) (pure False)
        cTimer = timer cTimerRst
        cTimerRst = mux (cTimer .>=. cPeriodNs) (pure True) (pure False)

        enD = mux (dTimer .>=. dPeriodNs) (pure True) (pure False)
        dTimer = timer dTimerRst
        dTimerRst = mux (dTimer .>=. dPeriodNs) (pure True) (pure False)

        bPeriodNs = 100000 -- 10Hz in ns
        cPeriodNs = 200000 -- 5Hz in ns
        dPeriodNs = 400000 -- 2.5Hz in ns


enableCheck :: HiddenClockResetEnable dom => Signal dom Int
enableCheck = timerResult
    where 
        timerResult = timer timerRst
        timerRst = mux (timerResult .>=. thold) (pure True) (pure False)
        thold = 200000000000


llc :: HiddenClockResetEnable dom => Signal dom (Data, Bool, Bool, Bool, Int) -> Signal dom Data -> Signal dom (Int, (Data, Bool), (Data, Bool), (Data, Bool))
llc controls stage = bundle (stage, resultB, resultC, resultD)
    where 
        resultB = bundle (outB, aktvB)
        resultC = bundle (outC, aktvC)
        resultD = bundle (outD, aktvD)
        -- b & c are in the evaluation layer 1
        outB = evaluateB ((stage .==. pure 1) .&&. enB) a
        outC = evaluateC ((stage .==. pure 1) .&&. enC) a
        -- d is in evaluation layer 2
        outD = evaluateD ((stage .==. pure 2) .&&. enD) (bundle (outB, outC))
        -- output stage
        (aktvB, aktvC, aktvD) = unbundle $ mux (stage .==. pure 3) (bundle (enB, enC, enD)) (bundle (pure False, pure False, pure False)) 
        (a, enB, enC, enD, _) = unbundle controls


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
    Signal MyDomain (Data, Bool) -> Signal MyDomain (Bool, (Data, Bool, Bool, Bool, Int), (Int, (Data, Bool), (Data, Bool), (Data, Bool)))
topEntity clk rst en input0 = exposeClockResetEnable (evaluator input0) clk rst en
