module CycleWithTrickyOffset where

import Clash.Prelude

---------------------------------------------------------------

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


---------------------------------------------------------------

type Data = Signed 32
type MemoryData = (Data, Bool, Bool, Bool, Bool)

memorySize :: Int
memorySize = 4


hlc :: HiddenClockResetEnable dom => Signal dom Data -> Signal dom Bool -> Signal dom (Data, Bool, Bool, Bool, Bool)
hlc x newX = bundle (x, newX, enableA, enableB, enableC)
    where 
        enableA = newX
        enableB = delay1Cycle enableA
        enableC = delay1Cycle enableB

        delay1Cycle :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
        delay1Cycle = register False



fifoQueue :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom Data -> Signal dom (Bool, Data, Data, Data, Data)
fifoQueue push pop dataIn = bundle (empty, outputBuffer, buffer0, buffer1, buffer2)
    where 
        empty = register True nextEmpty
        front = register (0 :: Int) nextFront
        buffer0 = register (0) nextBuffer0
        buffer1 = register (0) nextBuffer1
        buffer2 = register (0) nextBuffer2

        outputBuffer = mux (front .==. 0) buffer0 $ mux (front .==. 1) buffer0 $ mux (front .==. 2) buffer1 $ mux (front .==. 3) buffer2 buffer0

        nextFront = mux (push .&&. (front .<. 3)) (front + 1) $ mux (pop .&&. (front .>. 0)) (front - 1) front
        nextEmpty = mux (front .==. 0) (pure True) (pure False)

        nextBuffer0 = mux (push .&&. (front .<. 3)) dataIn buffer0
        nextBuffer1 = mux (push .&&. (front .<. 3)) buffer0 buffer1
        nextBuffer2 = mux (push .&&. (front .<. 3)) buffer1 buffer2
-- clashi Test:
-- Note: in sampleN reset is True for first 2 samples
-- let push = fromList [False, False, True, True, False, True, False]
-- let pop = fromList [False, False, False, False, True, False, True]
-- let dataIn = fromList [(0 :: Signed 32), 0,1,2,0,3,0]
-- sampleN @System 8 (fifoQueue push pop dataIn)


-- topEntity :: Clock System -> Reset System -> Enable System ->
--     Signal System Data -> Signal System Bool -> 
--     Signal System (Data, Data, (Data, Data, Data))
-- topEntity clk rst en x newX = bundle (a, b, c)
--     where 

--         a = exposeClockResetEnable (streamA enableA x c) clk rst en
--         b = exposeClockResetEnable (streamB enableB a) clk rst en
--         c = exposeClockResetEnable (streamC enableC b) clk rst en

--         enableA = exposeClockResetEnable pacingX clk rst en
--         enableB = exposeClockResetEnable (delay1Cycle pacingX) clk rst en
--         enableC = exposeClockResetEnable (delay2Cycles pacingX) clk rst en

--         pacingX = newX