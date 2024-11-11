module Experiments.AddStreams where

import Clash.Explicit.Testbench
import Clash.Prelude

mac :: (Num a) => a -> (a, a) -> (a, a)
mac acc (x, y) = (acc + x * y, acc)

macS :: (HiddenClockResetEnable dom, Num a, NFDataX a) => Signal dom (a, a) -> Signal dom a
macS = mealy mac 0

topEntity :: Clock System -> Reset System -> Enable System -> Signal System (Int, Int) -> Signal System Int
topEntity = exposeClockResetEnable macS

testbench :: Signal System Bool
testbench = done
  where
    clk = tbSystemClockGen (fmap not done)
    rst = systemResetGen
    en = enableGen
    testInput = stimuliGenerator clk rst ((1, 1) :> (2, 2) :> (3, 3) :> (4, 4) :> Nil)
    expectedOutput = outputVerifier' clk rst (0 :> 2 :> 4 :> 6 :> 8 :> Nil)
    done = expectedOutput (topEntity clk rst en testInput)

---------- WIP ---------------------

c :: (Num a, NFDataX a) => a -> a -> a
c x y = x + y



