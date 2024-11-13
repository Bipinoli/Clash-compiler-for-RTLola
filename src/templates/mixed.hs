module Mixed where

import Clash.Prelude

-- System domain -> 100 MHz clock

counter :: (HiddenClockResetEnable dom, KnownNat a) => Unsigned a -> Signal dom (Unsigned a)
counter toCount = register 0 (mux (prevVal .<. pure (toCount - 1)) (prevVal + 1) 0)
  where prevVal = counter toCount

generateClock :: HiddenClockResetEnable dom => Unsigned 64 -> Unsigned 64 -> Signal dom Bool
generateClock systemClockPeriod desiredPeriod = register False (mux (countSignal .==. maxVal) (pure True) (pure False))
  where
    countSignal = counter toCount
    toCount = desiredPeriod `div` systemClockPeriod
    maxVal = pure (toCount - 1)
-- Test:
-- clashi> sampleN @System 20 (generateClock 1 3)


-- getClockPeriod :: forall dom. HiddenClockResetEnable dom => Integer
-- getClockPeriod = snatToInteger (clockPeriod @dom)

-- generateClock2 :: HiddenClockResetEnable dom => Unsigned 64 -> Signal dom Bool
-- generateClock2 desiredFreq = register False (mux (countSignal .==. maxVal) (pure True) (pure False))
--   where
--     countSignal = counter toCount
--     toCount = systemClockFreq `div` desiredFreq
--     systemClockFreq = getClockPeriod clockPeriod dom
--     maxVal = pure (toCount - 1)
    


last4Values :: HiddenClockResetEnable dom => Signal dom (Signed 32) -> Signal dom (Signed 32, Signed 32, Signed 32, Signed 32)
last4Values inputSignal = bundle (curr, past0, past1, past2)
  where 
    curr = inputSignal
    past0 = register 0 curr
    past1 = register 0 past0
    past2 = register 0 past1
-- Test:
-- clashi> sampleN @System 9 (last4Values $ fromList [1,2,3,4,5,6,7,8,9,10])




streamB :: HiddenClockResetEnable dom => Signal dom (Signed 32) -> Signal dom (Signed 32)
streamB a = register 0 a

streamC :: HiddenClockResetEnable dom => Signal dom (Signed 32) -> Signal dom (Signed 32)
streamC b = register 0 (register 0 b)

windowB :: HiddenClockResetEnable dom => Signal dom (Signed 32) -> Signal dom (Signed 32, Signed 32, Signed 32, Signed 32)
windowB = last4Values

streamD :: HiddenClockResetEnable dom => Signal dom (Signed 32, Signed 32, Signed 32, Signed 32) -> Signal dom (Signed 32)
streamD winB = register 0 sumVal
  where 
    sumVal = fmap (\(x1, x2, x3, x4) -> x1 + x2 + x3 + x4) winB





topEntity :: Clock System -> Reset System -> Enable System -> Signal System (Signed 32) -> Signal System (Signed 32, Signed 32, Signed 32)
topEntity clk rst en a = bundle (strmB, strmC, strmD)
  where
    strmB = exposeClockResetEnable (streamB a) clk rst en
    strmC = exposeClockResetEnable (streamC strmB) clk rst en
    strmD = exposeClockResetEnable (streamD . windowB $ strmB) clk rst dEnable
    dEnable = toEnable $ exposeClockResetEnable (generateClock 1 2) clk rst en



