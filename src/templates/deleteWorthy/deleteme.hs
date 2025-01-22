import Clash.Prelude

-- hotPotato :: HiddenClockResetEnable dom => Int -> Signal dom Int
-- hotPotato n = register 0 (mux (hotPotato n .==. pure (n-1)) 0 (hotPotato n + 1))

hotPotato :: HiddenClockResetEnable dom => Int -> Signal dom Int
hotPotato n = let s = register 0 (mux (s .==. pure (n-1)) 0 (s + 1)) in s

topEntity :: Clock System -> Reset System -> Enable System -> Signal System Int
topEntity = exposeClockResetEnable (hotPotato 8)