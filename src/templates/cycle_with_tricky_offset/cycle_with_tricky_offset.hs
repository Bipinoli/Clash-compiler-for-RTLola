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

-- evaluator :: HiddenClockResetEnable dom => Signal dom (Int, Bool) -> Signal dom ((Int, Bool), (Int, Bool), (Int, Bool))
-- evaluator input0 = bundle (output0, output1, output2)
--     where
--         output0 = bundle (a, aktv_a)
--         output1 = bundle (b, aktv_b)
--         output2 = bundle (c, aktv_c)
--         (x, aktv_x) = unbundle input0
--         (a, aktv_a) = unbundle $ evaluateA enA (bundle (x, c))
--         (b, aktv_b) = unbundle $ evaluateB enB a
--         (c, aktv_c) = unbundle $ evaluateC enC b


hlc :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Int, Bool) -> Signal dom (Int, Bool, Bool, Bool)
hlc en input0 = register (0, False, False, False) (mux en newResult oldResult)
    where
        oldResult = hlc en input0
        newResult = bundle (x, enA, enB, enC)
        (x, aktv_x) = unbundle input0
        enA = aktv_x
        enB = aktv_x
        enC = aktv_x

-- We need to sustain control signals and data from HLC until all layers are completely evaluated and outputed
-- Hence, the LLC should run (n+1) times faster than HLC, where n = number of evaluation layers
-- We evaluate all the layers and in the end output the values together, hence +1 in (n+1)
-- By sustaining the HLC controls like this we also don't need any buffer or queue to interface them
llc :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Int, Bool, Bool, Bool) -> Signal dom Int -> Signal dom ((Int, Bool), (Int, Bool), (Int, Bool))
llc en controls stage = bundle (resultA, resultB, resultC)
    where 
        resultA = bundle (outA, aktvA)
        resultB = bundle (outB, aktvB)
        resultC = bundle (outC, aktvC)
        outA = evaluateA ((stage .==. pure 1) .&&. enA) (bundle (x, outC))
        outB = evaluateB ((stage .==. pure 2) .&&. enB) outA
        outC = evaluateC ((stage .==. pure 3) .&&. enC) outB
        (aktvA, aktvB, aktvC) = unbundle $ mux (stage .==. pure 0) (bundle (enA, enB, enC)) (bundle (pure False, pure False, pure False))
        (x, enA, enB, enC) = unbundle controls



evaluateA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Int, Int) -> Signal dom Int
evaluateA en inpt = register 0  (mux en (x + c) oldVal)
    where 
        (x, c) = unbundle inpt 
        oldVal = evaluateA en inpt

evaluateB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom Int
evaluateB en inpt = register 0 (mux en (a + 1) oldVal)
    where 
        oldVal = evaluateB en inpt
        a = inpt

evaluateC :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom Int
evaluateC en inpt = register 0 (mux en (b + 1) oldVal)
    where 
        oldVal = evaluateC en inpt
        b = inpt
