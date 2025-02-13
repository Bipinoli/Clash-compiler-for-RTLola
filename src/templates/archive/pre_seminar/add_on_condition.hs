module AddOnCondition where

import Clash.Prelude

c :: HiddenClockResetEnable dom => 
    Signal dom Bool -> Signal dom (Signed 64) -> Signal dom (Signed 64) -> 
    Signal dom (Signed 64)
c to_add a b = mux to_add (a + b) (a - b)

trigger0 :: HiddenClockResetEnable dom => Signal dom (Signed 64) -> Signal dom Bool
trigger0 c = c .>. 10

trigger1 :: HiddenClockResetEnable dom => Signal dom (Signed 64) -> Signal dom Bool
trigger1 c = c .<. 5

topEntity :: Clock System -> Reset System -> Enable System ->
    Signal System Bool -> Signal System (Signed 64) -> Signal System (Signed 64) -> 
    (Signal System Bool, Signal System Bool)
topEntity clk rst en to_add a b = exposeClockResetEnable (trigger0 x, trigger1 x) clk rst en
    where x = exposeClockResetEnable (c to_add a b) clk rst en


