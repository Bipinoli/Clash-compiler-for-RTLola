module OffsetHoldAggregate where

import Clash.Prelude

-- measured in picoseconds: 100 MHz = 10_000 ps
systemClockPeriod = snatToInteger $ clockPeriod @System

generateClock :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
generateClock desiredPeriod = register False (mux (countSignal .==. maxVal) (pure True) (pure False))
  where
    countSignal = counter toCount
    toCount = desiredPeriod `div` systemClockPeriod
    maxVal = pure (toCount - 1)

    counter :: HiddenClockResetEnable dom => Integer -> Signal dom Integer 
    counter toCount = register 0 (mux (prevVal .<. pure (toCount - 1)) (prevVal + 1) 0)
        where prevVal = counter toCount
-- Test: (generating the clock which is half as fast as the system clk)
-- clashi> sampleN @System 10 $ generateClock 20000 


bEnable :: HiddenClockResetEnable dom => Signal dom Bool
bEnable = generateClock 100_000_000 -- 10 kHZ

dEnable :: HiddenClockResetEnable dom => Signal dom Bool
dEnable = generateClock 200_000_000 -- 5 kHZ

evalSignal :: HiddenClockResetEnable dom => Signal dom Bool
evalSignal = clockReducer 10

