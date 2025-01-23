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
slowClock = clockDivider 4

fastClock :: HiddenClockResetEnable dom => Signal dom Bool
fastClock = clockDivider 1

-- We need to sustain control signals and data from HLC until all layers are completely evaluated and outputed
-- Hence, the LLC should run (n+1) times faster than HLC, where n = number of evaluation layers
-- We evaluate all the layers and in the end output the values together, hence +1 in (n+1)
-- By sustaining the HLC controls like this we also don't need any buffer or queue to interface them
evaluator :: HiddenClockResetEnable dom => Signal dom (Data, Bool) -> Signal dom ((Data, Bool), (Data, Bool), (Data, Bool))
evaluator input0 = outputs
    where
        outputs = llc fastClock controls stage
        controls = hlc slowClock input0
        stage = hotPotato 4


hlc :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Data, Bool) -> Signal dom (Data, Bool, Bool, Bool)
hlc en input0 = register (0, False, False, False) (mux en newResult oldResult)
    where
        oldResult = hlc en input0
        newResult = bundle (x, enA, enB, enC)
        (x, aktv_x) = unbundle input0
        enA = aktv_x
        enB = aktv_x
        enC = aktv_x


llc :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Data, Bool, Bool, Bool) -> Signal dom Data -> Signal dom ((Data, Bool), (Data, Bool), (Data, Bool))
llc en controls stage = bundle (resultA, resultB, resultC)
    where 
        resultA = bundle (outA, aktvA)
        resultB = bundle (outB, aktvB)
        resultC = bundle (outC, aktvC)
        outA = evaluateA ((stage .==. pure 1) .&&. enA) (bundle (x, outC'))
        outB = evaluateB ((stage .==. pure 2) .&&. enB) outA
        (outC', outC) = unbundle $ evaluateC ((stage .==. pure 3) .&&. enC) outB
        (aktvA, aktvB, aktvC) = unbundle $ mux (stage .==. pure 0) (bundle (enA, enB, enC)) (bundle (pure False, pure False, pure False))
        (x, enA, enB, enC) = unbundle controls



evaluateA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Data, Data) -> Signal dom Data
evaluateA en inpt = register 0  (mux en (x + c) oldVal)
    where 
        (x, c) = unbundle inpt 
        oldVal = evaluateA en inpt

evaluateB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data
evaluateB en inpt = register 0 (mux en (a + 1) oldVal)
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
