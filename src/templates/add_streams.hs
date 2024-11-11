module AddStreams where

import Clash.Prelude

c :: (KnownNat n, HiddenClockResetEnable dom) => 
    Signal dom (Signed n) -> Signal dom (Signed n) -> Signal dom (Signed n)
c a b = a + b

trigger0 :: (KnownNat n, HiddenClockResetEnable dom) => 
    Signal dom (Signed n) -> Signal dom Bool
trigger0 c = c .>. 2

topEntity :: Clock System -> Reset System -> Enable System -> 
    Signal System (Signed 64) -> Signal System (Signed 64) -> Signal System Bool
topEntity clk rst en a b = exposeClockResetEnable (trigger0 (c a b)) clk rst en
