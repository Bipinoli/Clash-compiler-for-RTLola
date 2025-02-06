module Queue where
    
import Clash.Prelude


-- type InputATyp = Bool
-- type InputBTyp = Bool
-- type InputCTyp = Signed 8

-- type QData = ((InputATyp, Bool), (InputBTyp, Bool), (InputCTyp, Bool))
-- qNullData = ((False, False), (False, False), (0, False)) :: QData


type QData = Int
qNullData = 0 :: QData

type QMemSize = 5

type QMem = Vec QMemSize QData
type QCursor = Int
type QPush = Bool
type QPop = Bool
type QPushValid = Bool
type QPopValid = Bool

type QState = (QMem, QCursor)
type QInput = (QPush, QPop, QData)
type QOutput = (QPushValid, QPopValid, QData, QState)


-- Note: assumed that queue never overflows
queueFx :: QState -> QInput -> (QState, QOutput)
queueFx state input = (nextState, output)
    where
        output = (pushValid, popValid, outData, nextState)
        nextState = (nextMem, nextCursor)
        nextMem = case (push, pop) of
            (True, _) -> qData +>> qMem
            (_, _) -> qMem
        outData = case (push, pop) of
            (_, True) -> if cursor == 0 then qNullData else qMem !! (cursor - 1)
            (_, _) -> qNullData
        nextCursor = case (push, pop) of
            (True, False) -> cursor + 1
            (False, True) -> if cursor == 0 then 0 else cursor - 1
            (_, _) -> cursor
        pushValid = case (push, pop) of
            (True, _) -> cursor /= length qMem
            (_, _) -> False
        popValid = case (push, pop) of 
            (_, True) -> cursor /= 0
            (_, _) -> False
        (qMem, cursor) = state
        (push, pop, qData) = input


queue :: HiddenClockResetEnable dom => Signal dom QInput -> Signal dom QOutput
queue = mealy queueFx (repeat qNullData :: QMem, 0 :: QCursor)


topEntity :: Clock System -> Reset System -> Enable System -> Signal System QInput -> Signal System QOutput
topEntity = exposeClockResetEnable queue

---------------------------------- TEST -----------------------------------

nullInput = (False, False, 0) :: QInput
input1 = (True, False, 10) :: QInput -- push 10
input2 = (True, False, 20) :: QInput -- push 20
input3 = (True, False, 30) :: QInput -- push 30
input4 = (True, True, 40) :: QInput -- push 40 & pop
input5 = (False, True, 0) :: QInput -- pop
input6 = (False, True, 0) :: QInput -- pop
input7 = (False, True, 0) :: QInput -- pop
input8 = (False, True, 0) :: QInput -- pop
input9 = (False, True, 0) :: QInput -- pop

-- sampleN sends reset signals for first 2 cycles so null inputs
inputs :: HiddenClockResetEnable dom => Signal dom QInput
inputs = fromList [nullInput, nullInput, input1, input2, nullInput, nullInput, input3, input4, nullInput, input5, input6, input7, input8, input9] 

outputs = sampleN @System 14 (queue inputs)