module SimpleEventBased where

import Clash.Prelude

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

clockDivider :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
clockDivider factor = generateClock $ systemClockPeriod * factor

----------------------------------------

pacingX1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom Bool
pacingX1 x1 x2 = x1

delay2Cycles :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
delay2Cycles = register False . register False


----------------------------------------

type Data = Signed 32


window3 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Data -> Signal dom (Data, Data, Data)
window3 enable x deflt = bundle (cur, past1, past2) 
  where 
    cur = register deflt (mux enable x cur)
    past1 = register deflt (mux enable cur past1)
    past2 = register deflt (mux enable past1 past2)
-- clashi Test:
-- let x = fromList [(1 :: Signed 32), 2,3,4,5,6,7,8]
-- let en = fromList [True, True, True, True, True, True, True, True]
-- sampleN @System 8 (window3 en x (-1))
-- Note: in sampleN reset is True for first 2 samples


streamA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data -> Signal dom Data
streamA enable x1 x2 = register 0 (mux enable (x1 + x2) oldVal)
    where oldVal = streamA enable x1 x2

streamB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom (Data, Data, Data)
streamB enable a = window3 enable a (-1)



----------------------------------------

topEntity :: Clock System -> Reset System -> Enable System ->
    Signal System Data -> Signal System Data -> 
    Signal System Bool -> Signal System Bool -> 
    Signal System (Data, (Data, Data, Data))
topEntity clk rst en x1 x2 newX1 newX2 = bundle (a, b)
    where 
        a = exposeClockResetEnable (streamA enableA holdX1 holdX2) clk rst en
        b = exposeClockResetEnable (streamB enableB a) clk rst en

        enableA = exposeClockResetEnable (delay2Cycles $ pacingX1 newX1 newX2) clk rst en
        enableB = enableA

        holdX1 = exposeClockResetEnable (register (-1) (mux newX1 x1 holdX1)) clk rst en
        holdX2 = exposeClockResetEnable (register (-1) (mux newX2 x2 holdX2)) clk rst en