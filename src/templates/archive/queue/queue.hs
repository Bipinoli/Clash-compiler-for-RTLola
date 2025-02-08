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
        -- keeping in registers to avoid any combinational output
        output = bundle (pushValid, popValid, outData, state)
        state = bundle (buffer, cursor)
        buffer = register (repeat 0 :: QMem) nextBufferSignal
        cursor = register 0 nextCursorSignal
        outData = register qNullData nextOutDataSignal
        pushValid = register False nextPushValidSignal
        popValid = register False nextPopValidSignal

        -- Signal is an applicative functor
        -- <$> = alias for fmap - wraps normal function into a function on Signal
        -- <*> = apply applicative - it applies signal of function to a signal of a value

        nextBufferSignal = nextBuffer <$> buffer <*> bundle (input, cursor)
        nextCursorSignal = nextCursor <$> cursor <*> bundle (input, buffer)
        nextOutDataSignal = nextOutData <$> bundle (input, cursor, buffer)
        nextPushValidSignal = nextPushValid <$> bundle (input, cursor, buffer)
        nextPopValidSignal = nextPopValid <$> bundle (input, cursor)
        
        nextBuffer :: QMem -> (QInput, QCursor) -> QMem
        nextBuffer buf ((push, pop, qData), cur) = out
            where 
                out = case (push, pop) of
                    (True, _) -> if cur /= length buf then qData +>> buf else buf
                    (_, _) -> buf

        nextCursor :: QCursor -> (QInput, QMem) -> QCursor
        nextCursor cur ((push, pop, _), buf) = out
            where 
                out = case (push, pop) of
                    (True, False) -> if cur /= length buf then cur + 1 else cur
                    (False, True) -> if cur /= 0 then cur - 1 else 0
                    (True, True) -> if cur == 0 then cur + 1 else cur 
                    (_, _) -> cur

        nextOutData :: (QInput, QCursor, QMem) -> QData
        nextOutData ((push, pop, _), cur, buf) = out
            where 
                out = case (push, pop) of
                    (_, True) -> if cur == 0 then qNullData else buf !! (cur - 1)
                    (_, _) -> qNullData 

        nextPushValid :: (QInput, QCursor, QMem) -> QPush
        nextPushValid ((push, pop, _), cur, buf) = out
            where 
                out = case (push, pop) of
                    (True, _) -> cur /= length buf
                    (_, _) -> False

        nextPopValid :: (QInput, QCursor) -> QPop
        nextPopValid ((push, pop, _), cur) = out
            where 
                out = case (push, pop) of
                    (_, True) -> cur /= 0
                    (_, _) -> False


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