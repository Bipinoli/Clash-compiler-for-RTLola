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

clockReducer :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
clockReducer factor = generateClock $ systemClockPeriod * factor

clk1 :: HiddenClockResetEnable dom => Signal dom Bool
clk1 = generateClock 40_000 -- 25 Mhz = 40_000 ps period

clk2 :: HiddenClockResetEnable dom => Signal dom Bool
clk2 = clockReducer 2 

topEntity :: Clock System -> Reset System -> Enable System -> Signal System (Bool, Bool)
topEntity = exposeClockResetEnable $ bundle (clk1, clk2)