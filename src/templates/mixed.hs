module FirFilter.FirFilter where

import Clash.Prelude
import Clash.Explicit.Testbench


firFilter :: (HiddenClockResetEnable dom, NFDataX a, Num a, KnownNat n) => Vec (n+1) a -> Signal dom a -> Signal dom a
firFilter coeffs xs = dotProduct (pure `fmap` coeffs) (iterateI (register 0) xs)
    where dotProduct as bs = fold (+) $ zipWith (*) as bs


topEntity :: Clock System -> Reset System -> Enable System -> Signal System (Signed 16) -> Signal System (Signed 16)
topEntity = exposeClockResetEnable $ firFilter (2 :> 2 :> 2 :> 2 :> 2 :> Nil)




testbench :: Signal System Bool
testbench = done
  where 
    clk = tbSystemClockGen (not <$> done)
    rst = systemResetGen
    en = enableGen
    inputs = stimuliGenerator clk rst (0 :> 5 :> 10 :> 5 :> 0 :> -5 :> -10 :> -5 :> 0 :> 5 :> 10 :> 5 :> 0 :> -5 :> -10 :> -5 :> 0 :> Nil)
    expectedOutput = outputVerifier' clk rst (0 :> 0 :> 0 :> 0 :> 0 :> 30 :> 0 :> -30 :> -40 :> -30 :> 0 :> 30 :> Nil)
    -- type level number SNat
    done = expectedOutput $ ignoreFor clk rst en (SNat :: SNat 5) 0 (topEntity clk rst en inputs)

-- Mark testbench as top entity to generate Verilog
{-# ANN testbench
  (Synthesize
    { t_name   = "testbench"
    , t_inputs = []
    , t_output = PortName "done"
    }) #-}

