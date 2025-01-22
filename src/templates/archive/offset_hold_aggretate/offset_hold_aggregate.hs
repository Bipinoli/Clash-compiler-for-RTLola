module OffsetHoldAggregate where

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


bEnable :: HiddenClockResetEnable dom => Signal dom Bool
bEnable = generateClock 100_000_000 -- 10 kHZ

dEnable :: HiddenClockResetEnable dom => Signal dom Bool
dEnable = generateClock 200_000_000 -- 5 kHZ

evalSignal :: HiddenClockResetEnable dom => Signal dom Bool
evalSignal = clockDivider 10


type Data = Signed 32

bufferB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom (Data, Data, Data, Data, Data)
bufferB enable a = bundle (cur, past1, past2, past3, past4)
    where 
        cur = register 0 (mux enable a cur)
        past1 = register 0 (mux enable cur past1)
        past2 = register 0 (mux enable past1 past2)
        past3 = register 0 (mux enable past2 past3)
        past4 = register 0 (mux enable past3 past4)


streamB :: HiddenClockResetEnable dom => Signal dom (Data, Data, Data, Data, Data) -> Signal dom Data
streamB bufB = item
  where (item, _, _, _, _) = unbundle bufB


streamC :: HiddenClockResetEnable dom => Signal dom (Data, Data, Data, Data, Data) -> Signal dom Data
streamC bufB = register 10 item
  where (_, _, item, _, _) = unbundle bufB


streamD :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Data, Data, Data, Data, Data) -> Signal dom Data
streamD enable bufB = register 0 (mux enable newSum oldSum)
  where
    newSum = fmap (\(x1, x2, x3, x4, x5) -> x1 + x2 + x3 + x4 + x5) bufB
    oldSum = streamD enable bufB

  
topEntity :: Clock System -> Reset System -> Enable System -> Signal System Data ->
  Signal System (Data, Data, Data, Data)
topEntity clk rst en a = bundle (a, b, c, d)
  where 
    b = exposeClockResetEnable (streamB bufB) clk rst en
    c = exposeClockResetEnable (streamC bufB) clk rst en
    d = exposeClockResetEnable (streamD dEnable bufB) clk rst en
    bufB = exposeClockResetEnable (bufferB bEnable a) clk rst en
