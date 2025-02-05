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

------- TEST -------

state0 :: (QMem, QCursor)
state0 = (repeat qNullData :: QMem, 0 :: QCursor)

input1 :: QInput
input1 = (True, False, 10)
(state1, output1) = queueFx state0 input1

input2 = (False, False, 0) :: QInput
(state2, output2) = queueFx state1 input2

input3 = (True, False, 20) :: QInput
(state3, output3) = queueFx state2 input3

input4 = (True, True, 30) :: QInput
(state4, output4) = queueFx state3 input4

input5 = (False, True, 0) :: QInput
(state5, output5) = queueFx state4 input5

input6 = (False, True, 0) :: QInput
(state6, output6) = queueFx state5 input6

input7 = (False, True, 0) :: QInput
(state7, output7) = queueFx state6 input7


--------------------


queue :: HiddenClockResetEnable dom => Signal dom QInput -> Signal dom QOutput
queue = mealy queueFx (repeat qNullData :: QMem, 0 :: QCursor)


-------------------------------------------------------------------------

topEntity :: Clock System -> Reset System -> Enable System -> Signal System QInput -> Signal System QOutput
topEntity = exposeClockResetEnable queue
