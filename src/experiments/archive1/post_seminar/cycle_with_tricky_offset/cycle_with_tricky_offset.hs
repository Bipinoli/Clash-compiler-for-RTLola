module CycleWithTrickyOffset where

import Clash.Prelude


---------------------------------------------------------------

systemClockPeriod = snatToInteger $ clockPeriod @System

generateClock :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
generateClock desiredPeriodInPs = register False (mux (countSignal .<. halfToCount) (pure True) (pure False))
  where
    countSignal = counter toCount
    toCount = desiredPeriodInPs `div` systemClockPeriod
    halfToCount = pure $ toCount `div` 2

    counter :: HiddenClockResetEnable dom => Integer -> Signal dom Integer
    counter toCount = register 0 (mux (prevVal .<. pure (toCount - 1)) (prevVal + 1) 0)
        where prevVal = counter toCount

clockDivider :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
clockDivider factor = generateClock $ systemClockPeriod * factor


---------------------------------------------------------------

hotPotato :: HiddenClockResetEnable dom => Int -> Signal dom Int
hotPotato n = let s = register 0 (mux (s .==. pure (n-1)) 0 (s + 1)) in s
-- clashi Test:
-- Note: in sampleN reset is True for first 2 samples
-- sampleN @System 25 (hotPotato 8)


---------------------------------------------------------------

type Data = Int

slowClock :: HiddenClockResetEnable dom => Signal dom Bool
slowClock = clockDivider 5

-- We need to sustain control signals and data from HLC until all layers are completely evaluated and outputed
-- It talke one cycle for RTL to HLC and we want one cycle in the end to output the evaluated streams
-- Hence, the LLC should run (n+2) times faster than HLC, where n = number of evaluation layers
-- By sustaining the HLC controls like this we also don't need any buffer or queue to interface them

-- It is important to note that the input data must be proided by stage = 0 to work with new data on the current LLC loop
evaluator :: HiddenClockResetEnable dom => Signal dom (Data, Bool) -> Signal dom (Bool, (Data, Bool, Bool, Bool), (Int, (Data, Bool), (Data, Bool), (Data, Bool)))
evaluator input0 = bundle(slowClock, controls, outputs)
    where
        outputs = llc controls stage
        controls = hlc slowClock input0
        stage = delay 0 (hotPotato 5)


hlc :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Data, Bool) -> Signal dom (Data, Bool, Bool, Bool)
hlc en input0 = register (0, False, False, False) (mux en newResult oldResult)
    where
        oldResult = hlc en input0
        newResult = bundle (x, enA, enB, enC)
        (x, aktv_x) = unbundle input0
        enA = aktv_x
        enB = aktv_x
        enC = aktv_x


llc :: HiddenClockResetEnable dom => Signal dom (Data, Bool, Bool, Bool) -> Signal dom Data -> Signal dom (Int, (Data, Bool), (Data, Bool), (Data, Bool))
llc controls stage = bundle (outputPhase, resultA, resultB, resultC)
    where 
        resultA = bundle (outA, aktvA)
        resultB = bundle (outB, aktvB)
        resultC = bundle (outC, aktvC)
        outA = evaluateA ((stage .==. pure 1) .&&. enA) (bundle (x, outC))
        outB = evaluateB ((stage .==. pure 2) .&&. enB) outA
        -- Since c will be only calculated in the end we need to only keep the latest value from past
        -- no need to keep the window of 2
        (outC', outC) = unbundle $ evaluateC ((stage .==. pure 3) .&&. enC) outB
        (aktvA, aktvB, aktvC) = unbundle $ mux (stage .==. pure 4) (bundle (enA, enB, enC)) (bundle (pure False, pure False, pure False))
        -- outputPhase  = mux (stage .==. pure 3) (pure True) (pure False)
        outputPhase = stage
        (x, enA, enB, enC) = unbundle controls



evaluateA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Data, Data) -> Signal dom Data
evaluateA en inpt = register 100  (mux en (x + c) oldVal)
    where 
        (x, c) = unbundle inpt 
        oldVal = evaluateA en inpt

evaluateB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data
evaluateB en inpt = register 100 (mux en (a + 1) oldVal)
    where 
        oldVal = evaluateB en inpt
        a = inpt

evaluateC :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom (Data, Data)
evaluateC en inpt = window2 en (b + 1) 0
    where 
        b = inpt

window2 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Data -> Signal dom (Data, Data)
window2 enable x deflt = bundle (past1, cur)
  where
    cur = register deflt (mux enable x cur)
    past1 = register deflt (mux enable cur past1)


---------------------------------------------------------------


topEntity :: Clock System -> Reset System -> Enable System -> 
    Signal System (Data, Bool) -> Signal System (Bool, (Data, Bool, Bool, Bool), (Int, (Data, Bool), (Data, Bool), (Data, Bool)))
topEntity clk rst en input0 = exposeClockResetEnable (evaluator input0) clk rst en
