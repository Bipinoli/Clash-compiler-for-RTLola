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

type MemorySize = 4
type MemoryData = Data
type Cursor = Int
type Empty = Bool
type Memory = Vec MemorySize MemoryData
type MemoryState = (Memory, Cursor, Empty)
type Push = Bool
type Pop = Bool
type MemoryInput = (Push, Pop, MemoryData)
type ValidOutput = Bool
type MemoryOutput = (Empty, ValidOutput, MemoryData)

fifoFx :: MemoryState -> MemoryInput -> (MemoryState, MemoryOutput)
fifoFx memoryState memoryInput = (nextMemoryState, output)
    where 
        (push, pop, memData) = memoryInput
        (memory, cursor, isEmpty) = memoryState
        nextMemoryState = (newMemory, newCursor, newIsEmpty)
        output = (newIsEmpty, validOutput, outputData)
        newMemory = case (push, pop) of
            (True, False) -> memData +>> memory 
            (_, _) -> memory
        (newCursor, newIsEmpty, validOutput) = case (push, pop) of 
            (True, False) -> if cursor == length memory then (cursor, False, False) else (cursor + 1, False, False)
            (False, True) -> if cursor == 0 || isEmpty then (cursor, True, False) else if cursor == 1 then (cursor - 1, True, True) else (cursor - 1, False, True)
            (_, _) -> (cursor, isEmpty, False)
        outputData = case (push, pop) of
            (False, True) -> if cursor > 0 then memory !! (cursor - 1) else head memory
            (_, _) -> memData

queue :: HiddenClockResetEnable dom => Signal dom MemoryInput -> Signal dom MemoryOutput
queue = mealy fifoFx (repeat 0 :: Memory, 0 :: Cursor, True :: Empty)
-- clashi Test:
-- Note: in sampleN reset is True for first 2 samples
-- let x = fromList [(False, False, 0) :: MemoryInput, (False, False, 0), (True, False, 1), (True, False, 2), (True, False, 3), (False, True, 0), (True, False, 4), (True, False, 5), (True, False, 6), (True, False, 7), (False, True, 0), (False, True, 0), (False, True, 0), (False, True, 0), (False, True, 0)]
-- sampleN @System 15 (fifo x)
        

---------------------------------------------------------------


hlc :: HiddenClockResetEnable dom => Signal dom Data -> Signal dom Bool -> Signal dom (Data, Bool, Bool, Bool, Bool)
hlc x newX = bundle (x, newX, enableA, enableB, enableC)
    where 
        enableA = newX
        enableB = delay1Cycle enableA
        enableC = delay1Cycle enableB

        delay1Cycle :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
        delay1Cycle = register False



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