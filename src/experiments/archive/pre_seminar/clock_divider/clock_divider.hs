module ClockDivider where

import Clash.Prelude

-- measured in picoseconds: 100 MHz = 10_000 ps
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
-- Test: (generating the clock which is half as fast as the system clk)
-- clashi> sampleN @System 10 $ generateClock 20000 

clockDivider :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
clockDivider factor = generateClock $ systemClockPeriod * factor

clk1 :: HiddenClockResetEnable dom => Signal dom Bool
clk1 = generateClock 40_000 -- 25 Mhz = 40_000 ps period

clk2 :: HiddenClockResetEnable dom => Signal dom Bool
clk2 = clockDivider 2 

clk3 :: HiddenClockResetEnable dom => Signal dom Bool
clk3 = clockDivider 5 

clk4 :: HiddenClockResetEnable dom => Signal dom Bool
clk4 = clockDivider 6 

clk5 :: HiddenClockResetEnable dom => Signal dom Bool
clk5 = clockDivider 1 

topEntity :: Clock System -> Reset System -> Enable System -> Signal System (Bool, Bool, Bool, Bool, Bool)
topEntity = exposeClockResetEnable $ bundle (clk1, clk2, clk3, clk4, clk5)