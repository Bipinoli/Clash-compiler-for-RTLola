module CycleWithTrickyOffset where

import Clash.Prelude

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

defaultVal :: MemoryData
defaultVal = 0

fifoFx :: MemoryState -> MemoryInput -> (MemoryState, MemoryOutput)
fifoFx memoryState memoryInput = (nextMemoryState, output)
    where
        -- when we get new data the data may be available for only 1 cycle
        -- we must immediately put the data to the queue 
        -- so the queue must support push and pop at the same instant
        (push, pop, memData) = memoryInput
        (memory, cursor, isEmpty) = memoryState
        nextMemoryState = (newMemory, newCursor, newIsEmpty)
        output = (newIsEmpty, validOutput, outputData)
        newMemory = case (push, pop) of
            (True, False) -> memData +>> memory
            (True, True) -> memData +>> memory
            (_, _) -> memory
        (newCursor, newIsEmpty, validOutput) = case (push, pop) of
            (True, False) -> if cursor == length memory then (cursor, False, False) else (cursor + 1, False, False)
            (False, True) -> if cursor == 0 || isEmpty then (cursor, True, False) else if cursor == 1 then (cursor - 1, True, True) else (cursor - 1, False, True)
            (True, True) -> if cursor == 0 || isEmpty then (cursor + 1, False, False) else (cursor, False, True)
            (_, _) -> (cursor, isEmpty, False)
        outputData = case (push, pop) of
            (False, True) -> if cursor > 0 then memory !! (cursor - 1) else head memory
            (True, True) -> if cursor > 0 then memory !! (cursor - 1) else head memory
            (_, _) -> memData

queue :: HiddenClockResetEnable dom => Signal dom MemoryInput -> Signal dom MemoryOutput
queue = mealy fifoFx (repeat defaultVal :: Memory, 0 :: Cursor, True :: Empty)
-- clashi Test:
-- Note: in sampleN reset is True for first 2 samples
-- assuming `type MemorySize = 4` and `type MemoryData = Data`
-- let x = fromList [(False, False, 0) :: MemoryInput, (False, False, 0), (True, False, 1), (True, False, 2), (True, False, 3), (False, True, 0), (True, False, 4), (True, False, 5), (True, False, 6), (True, False, 7), (False, True, 0), (False, True, 0), (False, True, 0), (False, True, 0), (False, True, 0)]
-- sampleN @System 15 (fifo x)


---------------------------------------------------------------

hotPotato :: HiddenClockResetEnable dom => Int -> Signal dom Int
hotPotato n = let s = register 0 (mux (s .==. pure (n-1)) 0 (s + 1)) in s
-- clashi Test:
-- Note: in sampleN reset is True for first 2 samples
-- sampleN @System 25 (hotPotato 8)


---------------------------------------------------------------

-- -- NOTE: How to handle pacing & step by step layerwise evaluation together
-- -- Idea 1: Run evalution at higher frequency than pacing (similar idea to existing compiler)
-- -- Idea 2: Predict when the stream will be evaluatated and provide the dealayed pacing matching that -> whole thing can be run in higher frequency
-- evaluator :: HiddenClockResetEnable dom => Signal dom (Int, Bool) -> Signal dom ((Int, Bool), (Int, Bool), (Int, Bool))
-- evaluator input0 = bundle (output0, output1, output2)
--     where
--         output0 = (a, aktv_a)
--         output1 = (b, aktv_b)
--         output2 = (c, aktv_c)
--         (x, aktv_x) = input0
--         (a, aktv_a) = evaluateA enA (x, c)
--         (b, aktv_b) = evaluateB enb a
--         (c, aktv_c) = evaluateC enC b
--         enA = aktv_x
--         enB = delay False enA
--         enC = delay False enB


evaluateA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Int, Int) -> Signal dom (Int, Bool)
evaluateA en inpt = register (0, False) (mux en newResult oldResult)
    where 
        newResult = bundle (newVal, pure True)
        oldResult = bundle (oldVal, pure False)
        newVal = x + c
        (x, c) = unbundle inpt 
        (oldVal, _) = unbundle $ evaluateA en inpt

-- evaluateB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom Int
-- evaluateB en inpt = register (0, False) (mux en newResult oldResult)
--     where
--         newResult = bundle (newVal, True)
--         oldResult = bundle (oldVal, False)
--         (oldVal, _) = evaluateB en inpt
--         (newVal, _) = bundle (inpt + 1, pure True)


