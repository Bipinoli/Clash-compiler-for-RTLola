module SimpleCycle where

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

delay1Cycle :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
delay1Cycle = register False

delay2Cycles :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
delay2Cycles = register False . register False

delay3Cycles :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
delay3Cycles = register False . register False . register False

----------------------------------------

type Data = Signed 32

window3 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Data -> Signal dom (Data, Data, Data)
window3 enable x deflt = bundle (cur, past1, past2) 
  where 
    cur = register deflt (mux enable x cur)
    past1 = register deflt (mux enable cur past1)
    past2 = register deflt (mux enable past1 past2)


streamA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom (Data, Data, Data) -> Signal dom Data
streamA enable x c = register 0 (mux enable (x + offsetVal) oldVal)
    where 
        (_, offsetVal, _) = unbundle c
        oldVal = streamA enable x c

streamB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data
streamB enable a = register 0 (mux enable (a + 1) oldVal)
    where oldVal = streamB enable a

streamC :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom (Data, Data, Data)
streamC enable b = window3 enable (b + 1) 0

----------------------------------------

topEntity :: Clock System -> Reset System -> Enable System ->
    Signal System Data -> Signal System Bool -> 
    Signal System (Data, Data, (Data, Data, Data))
topEntity clk rst en x newX = bundle (a, b, c)
    where 
        a = exposeClockResetEnable (streamA enableA x c) clk rst en
        b = exposeClockResetEnable (streamB enableB a) clk rst en
        c = exposeClockResetEnable (streamC enableC b) clk rst en

        enableA = exposeClockResetEnable (pacingX) clk rst en
        enableB = exposeClockResetEnable (delay1Cycle pacingX) clk rst en
        enableC = exposeClockResetEnable (delay2Cycles pacingX) clk rst en

        pacingX = newX