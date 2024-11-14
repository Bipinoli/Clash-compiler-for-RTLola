module Mixed2 where

import Clash.Prelude

-- System domain -> 100 MHz clock
-- in nano seconds -> 10000 ns
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

clockReducer :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
clockReducer factor = generateClock $ systemClockPeriod * factor


-------------- Design notes ---------
-- it takes 1 cycle for register transfer 
-- let's keep a ghost register where the value will be inputted without caring for it's pacing
-- and as per the pacing the enable signal will be generated to pull the value from the ghost

-- on a right time emit enable signals to schedule the streams evaluation

b_ghost :: HiddenClockResetEnable dom => Signal dom (Signed 32) -> Signal dom (Signed 32)
b_ghost a = register 0 a

b_enable :: HiddenClockResetEnable dom => Enable dom
b_enable = toEnable $ generateClock 1000_000_000 -- 1 Hz -> 1 sec -> 10^9 ns

d_enable :: HiddenClockResetEnable dom => Enable dom
d_enable = toEnable $ generateClock 2000_000_000 -- 0.5 Hz -> 2 sec -> 20^9 ns





