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

type MemorySize = 3
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

hlc :: HiddenClockResetEnable dom => (Signal dom MemoryInput -> Signal dom MemoryOutput) -> Signal dom Data -> Signal dom Bool -> (Signal dom MemoryInput -> Signal dom MemoryOutput) 
hlc q x newX = q
    where 
        (newData, sX, sNewX, enableA, enableB, enableC) = unbundle (scheduler x newX)
        -- push to queue if newData 
        _ = mux newData (q pushInstr) (q placeholderInstr)
        pushInstr = bundle (pure True, pure False, memData) :: Signal dom MemoryInput
        placeholderInstr = bundle (pure False, pure False, memData) :: Signal dom MemoryInput
        memData = bundle (sX, sNewX, enableA, enableB, enableC) :: Signal dom MemoryData



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

popSignal :: HiddenClockResetEnable dom => Int -> Signal dom Bool
popSignal layers = sig
    where
        -- state machine (round robin through layers)
        -- 0 -> layer 0 (pop)
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


llc :: HiddenClockResetEnable dom => (Signal dom MemoryInput -> Signal dom MemoryOutput) -> Signal dom (Data, Data, (Data, Data))
llc q = evalResult
    where 
        -- pop instruction at every pop signal
        instr = mux (popSignal 4) (pure popInstr) (pure placeholderInstr)
        popInstr = (False, True, defaultVal) :: MemoryInput
        placeholderInstr = (False, False, defaultVal) :: MemoryInput

        (qEmpty, validOutput, memData) = unbundle (q instr)
        evalData = mux validOutput memData (pure defaultVal)

        evalResult = evaluator evalData

---------------------------------------------------------------


-- topEntity :: Clock System -> Reset System -> Enable System ->
--     Signal System Data -> Signal System Bool -> 
--     Signal System (Data, Data, (Data, Data))
-- topEntity clk rst en x newX = streamOutputs
--     where 
--         q = exposeClockResetEnable queue clk rst en

        
--         streamOutputs = exposeClockResetEnable (llc q) clk rst en