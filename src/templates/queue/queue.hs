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
queue :: HiddenClockResetEnable dom => Signal dom QInput -> Signal dom QOutput
queue input = output
    where 
        -- Keeping separate mealy machines so that they have their own registers 
        -- Otherwise some of them might be purely combinational which we don't want
        output = bundle (pushValid, popValid, outData, state)
        state = bundle (buffer, cursor)
        buffer = mealy nextBuffer (repeat 0 :: QMem) (bundle (input, cursor))
        cursor = mealy nextCursor 0 input
        outData = mealy nextOutData qNullData (bundle (input, cursor, buffer))
        pushValid = mealy nextPushValid False (bundle (input, cursor, buffer))
        popValid = mealy nextPopValid False (bundle (input, cursor))
        
        nextBuffer :: QMem -> (QInput, QCursor) -> (QMem, QMem)
        nextBuffer buf ((push, pop, qData), cur) = (out, out)
            where 
                out = case (push, pop) of
                    (True, _) -> if cur /= length buf then qData +>> buf else buf
                    (_, _) -> buf

        nextCursor :: QCursor -> QInput -> (QCursor, QCursor)
        nextCursor cur (push, pop, _) = (out, out)
            where 
                out = case (push, pop) of
                    (True, False) -> cur + 1
                    (False, True) -> if cur == 0 then 0 else cur - 1
                    (_, _) -> cur

        nextOutData :: QData -> (QInput, QCursor, QMem) -> (QData, QData)
        nextOutData _ ((push, pop, _), cur, buf) = (out, out)
            where 
                out = case (push, pop) of
                    (_, True) -> if cur == 0 then qNullData else buf !! (cur - 1)
                    (_, _) -> qNullData 

        nextPushValid :: QPush -> (QInput, QCursor, QMem) -> (QPush, QPush)
        nextPushValid _ ((push, pop, _), cur, buf) = (out, out)
            where 
                out = case (push, pop) of
                    (_, True) -> cur /= length buf
                    (_, _) -> False

        nextPopValid :: QPop -> (QInput, QCursor) -> (QPop, QPop)
        nextPopValid _ ((push, pop, _), cur) = (out, out)
            where 
                out = case (push, pop) of
                    (_, True) -> cur /= 0
                    (_, _) -> False

        -- cursor = register 0 nextCursor
        -- outData = register qNullData nextOutData
        -- pushValid = register False nextPushValid
        -- popValid = register False nextPopValid
        -- Signal is an applicative functor
        -- <$> = alias for fmap - it takes normal function to be a function on Signal
        -- <*> = apply applicative - it applies signal of function to a signal of a value
        -- nextBuffer = case (push, pop) of
        --     (True, _) -> let updatedBuf = (+>>) <$> qData <*> buffer in mux (cursor ./=. (length <$> buffer)) updatedBuf buffer
        --     (_, _) -> buffer
        -- nextOutData = case (push, pop) of 
        --     (_, True) -> if cursor .==. pure 0 then pure qNullData else (!!) <$> buffer <*> (cursor - 1)
        --     (_, _) -> pure qNullData
        -- nextCursor = case (push, pop) of 
        --     (True, False) -> if cursor ./=. (length <$> buffer) then pure (cursor + 1) else pure cursor
        --     (False, True) -> if cursor .==. pure 0 then pure 0 else pure (cursor - 1)
        -- nextPushValid = case (push, pop) of
        --     (True, _) -> cursor ./=. (length <$> buffer)
        --     (_, _) -> pure False
        -- nextPopValid = case (push, pop) of
        --     (_, True) -> cursor ./=. pure 0
        --     (_, _) -> pure False


-- -- Note: assumed that queue never overflows
-- queueFx :: QState -> QInput -> (QState, QOutput)
-- queueFx state input = (nextState, output)
--     where
--         output = (pushValid, popValid, outData, nextState)
--         nextState = (nextMem, nextCursor)
--         nextMem = case (push, pop) of
--             (True, _) -> qData +>> qMem
--             (_, _) -> qMem
--         outData = case (push, pop) of
--             (_, True) -> if cursor == 0 then qNullData else qMem !! (cursor - 1)
--             (_, _) -> qNullData
--         nextCursor = case (push, pop) of
--             (True, False) -> cursor + 1
--             (False, True) -> if cursor == 0 then 0 else cursor - 1
--             (_, _) -> cursor
--         pushValid = case (push, pop) of
--             (True, _) -> cursor /= length qMem
--             (_, _) -> False
--         popValid = case (push, pop) of 
--             (_, True) -> cursor /= 0
--             (_, _) -> False
--         (qMem, cursor) = state
--         (push, pop, qData) = input


-- queue :: HiddenClockResetEnable dom => Signal dom QInput -> Signal dom QOutput
-- queue = mealy queueFx (repeat qNullData :: QMem, 0 :: QCursor)


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