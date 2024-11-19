module DiffFreqsSimple where


import Clash.Prelude
import Clash.Explicit.Testbench


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

clockReducer :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
clockReducer factor = generateClock $ systemClockPeriod * factor


bEnable :: HiddenClockResetEnable dom => Signal dom Bool
bEnable = generateClock 100_000_000 -- 10 kHZ

cEnable :: HiddenClockResetEnable dom => Signal dom Bool
cEnable = generateClock 200_000_000 -- 5 kHZ

dEnable :: HiddenClockResetEnable dom => Signal dom Bool
dEnable = generateClock 400_000_000 -- 2.5 kHZ

evalSignal :: HiddenClockResetEnable dom => Signal dom Bool
evalSignal = clockReducer 10


streamB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom (Signed 32) -> Signal dom (Signed 32)
streamB eval enable a = register 10 (mux (eval .&&. enable) a oldVal)
    where oldVal = streamB eval enable a

streamC :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom (Signed 32) -> Signal dom (Signed 32)
streamC eval enable a = register 10 (mux (eval .&&. enable) a oldVal)
    where oldVal = streamC eval enable a

streamD :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Signed 32) -> Signal dom (Signed 32) -> Signal dom (Signed 32)
streamD enable a b = a + b


topEntity :: Clock System -> Reset System -> Enable System -> Signal System (Signed 32) -> Signal System (Bool, Signed 32, Signed 32, Signed 32, Signed 32)
topEntity clk rst en a = bundle (eval, a, b, c, d)
    where 
        b = exposeClockResetEnable (streamB eval bEnable a) clk rst en
        c = exposeClockResetEnable (streamC eval cEnable a) clk rst en
        d = exposeClockResetEnable (streamD dEnable b c) clk rst en
        eval = exposeClockResetEnable evalSignal clk rst en