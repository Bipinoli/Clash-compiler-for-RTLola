module Mixed where

import Clash.Prelude

systemClockFreq = 50000000 -- 50 MHz

counter :: (HiddenClockResetEnable dom, KnownNat a) => Unsigned a -> Signal dom (Unsigned a)
counter toCount = register 0 (mux (prevVal .<. pure (toCount - 1)) (prevVal + 1) 0)
  where prevVal = counter toCount

generateClock :: HiddenClockResetEnable dom => Unsigned 64 -> Unsigned 64 -> Signal dom Bool
generateClock systemClockFreq desiredFreq = register False (mux (countSignal .==. maxVal) (pure True) (pure False))
  where
    countSignal = counter toCount
    toCount = systemClockFreq `div` desiredFreq
    maxVal = pure (toCount - 1)
-- Test:
-- clashi> sampleN @System 20 (generateClock 3 1)


topEntity :: Clock System -> Reset System -> Enable System -> Unsigned 32 -> Signal System (Unsigned 32)
topEntity = exposeClockResetEnable counter

