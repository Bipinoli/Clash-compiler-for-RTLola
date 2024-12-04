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
type MemoryData = (Data, Bool, Bool, Bool, Bool)
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
defaultVal = (0, False, False, False, False)

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

scheduler :: HiddenClockResetEnable dom => Signal dom Data -> Signal dom Bool -> Signal dom (Bool, Data, Bool, Bool, Bool, Bool)
scheduler x newX = bundle (newData, x, newX, enableA, enableB, enableC)
    where
        enableA = newX
        enableB = delay1Cycle enableA
        enableC = delay1Cycle enableB
        newData = newX

        delay1Cycle :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
        delay1Cycle = register False
-- let x = fromList [0 :: Data, 0,1,2,3,4,5,6,7,8,9]
-- let newX = fromList [False,False,True,False,False,True,True,True,False,False,False]
-- sampleN @System 11 (scheduler x newX)


---------------------------------------------------------------

window2 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Data -> Signal dom (Data, Data)
window2 enable x deflt = bundle (cur, past1)
  where
    cur = register deflt (mux enable x cur)
    past1 = register deflt (mux enable cur past1)

streamA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom (Data, Data) -> Signal dom Data
streamA enable x c = register 0 (mux enable (x + offsetC) oldVal)
    where
        (_, offsetC) = unbundle c
        oldVal = streamA enable x c

streamB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data
streamB enable a = register 0 (mux enable (a + 1) oldVal)
    where oldVal = streamB enable a

streamC :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom (Data, Data)
streamC enable b = window2 enable (b + 1) 0

newEvalCycle :: HiddenClockResetEnable dom => Int -> Signal dom Bool
newEvalCycle layers = sig
    where
        -- we should pop from the queue after all layers have been evaluated
        -- layers are evaluated one after another as the higher layer depends on the result from the lower levels
        -- state machine (round robin through layers)
        -- 0 -> layer 0 (pop from queue)
        -- 1 -> layer 1 
        -- 2 -> layer 2 
        -- ...
        curState = register 0 newState
        newState = nextState (pure layers) curState

        sig = register True (newState .==. 0)

        nextState :: HiddenClockResetEnable dom => Signal dom Int -> Signal dom Int -> Signal dom Int
        nextState layersCount state = mux (state .==. (layersCount - 1)) (pure 0) (state + 1)


evaluator :: HiddenClockResetEnable dom => Signal dom MemoryData -> Signal dom (Data, Data, (Data, Data))
evaluator memData = bundle (a, b, c)
    where 
        a = streamA enableA x c
        b = streamB enableB a
        c = streamC enableC b
        (x, newX, enableA, enableB, enableC) = unbundle memData
-- clashi test
-- let x = fromList [(0,False,False,False,False) :: MemoryData, (0,False,False,False,False), (1,True,True,False,False), (0,False,False,True,False), (0,False,False,False,True), (2,True,True,False,False)]
-- sampleN @System 6 (evaluator x)


---------------------------------------------------------------

machine :: HiddenClockResetEnable dom => Signal dom Data -> Signal dom Bool -> Signal dom (Data, Data, (Data, Data))
machine x newX = result
    where 
        (newData, schX, schNewX, enableA, enableB, enableC) = unbundle (scheduler x newX)
        (empty, validOutput, memData) = unbundle (queue qInstr)
        result = evaluator (mux validOutput memData (pure defaultVal))

        qInstr = bundle (toPush, toPop, inptData)

        toPush = mux pushToQ (pure True) (pure False)
        toPop = mux popFromQ (pure True) (pure False)
        inptData = mux pushToQ scheduledData (pure defaultVal)
        scheduledData = bundle (schX, schNewX, enableA, enableB, enableC) 

        pushToQ = newData
        popFromQ = newEvalCycle 4

        
---------------------------------------------------------------


topEntity :: Clock System -> Reset System -> Enable System ->
    Signal System Data -> Signal System Bool -> 
    Signal System (Data, Data, (Data, Data))
topEntity clk rst en x newX = exposeClockResetEnable (machine x newX) clk rst en