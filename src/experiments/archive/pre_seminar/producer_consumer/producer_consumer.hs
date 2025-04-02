module ProducerConsumer where

import Clash.Prelude

type Data = Signed 32

type MemorySize = 3
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
queue = mealy fifoFx (repeat defaultVal :: Memory, 0 :: Cursor, True :: Empty)


producer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom MemoryInput
producer enable = mux enable pushInstr placeholderInstr
    where 
        pushInstr = bundle (pure True, pure False, inptNum)
        placeholderInstr = bundle (pure False, pure False, pure defaultVal)
        inptNum = register 0 (mux enable (inptNum + 1) inptNum)


consumer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom MemoryInput
consumer enable = mux enable popInstr placeholderInstr
    where 
        popInstr = bundle (pure False, pure True, pure defaultVal)
        placeholderInstr = bundle (pure False, pure False, pure defaultVal)


machine :: HiddenClockResetEnable dom => Signal dom MemoryOutput
machine = result
    where 
        result = queue inpt 
        enableConsumer = popSignal
        enableProducer = not <$> popSignal
        inpt = mux popSignal (consumer enableConsumer) (producer enableProducer)
-- Clashi Test: sampleN @System 10 machine


popSignal :: HiddenClockResetEnable dom => Signal dom Bool
popSignal = sig
    where
        curState = register 0 newState
        newState = nextState (pure 2) curState

        sig = register True (newState .==. 0)

        nextState :: HiddenClockResetEnable dom => Signal dom Int -> Signal dom Int -> Signal dom Int
        nextState statesCount state = mux (state .==. (statesCount - 1)) (pure 0) (state + 1)