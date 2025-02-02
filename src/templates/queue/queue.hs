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
type QOutput = (QPushValid, QPopValid, QData)


-- Note: assumed that queue never overflows
queueFx :: QState -> QInput -> (QState, QOutput)
queueFx state input = (nextState, output)
    where
        output = (pushValid, popValid, outData)
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


-------------------------------------------------------------------------

topEntity :: Clock System -> Reset System -> Enable System -> Signal System QInput -> Signal System QOutput
topEntity = exposeClockResetEnable queue
