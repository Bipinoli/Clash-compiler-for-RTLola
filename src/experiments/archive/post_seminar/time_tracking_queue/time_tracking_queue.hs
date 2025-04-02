--- Main idea:
--- It is a queue that allows for regular push, pop functions allowing push and pop to happen at the same time
--- In addition to that it also tracks how long the item stays in the queue before being popped
--- This way we can buffer the item in the queue and also calculate the actual time of arrival when we consume the item by popping
module TimeTrackingQueue where
    
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
type QWait = Vec QMemSize Int
type QCursor = Int
type QPush = Bool
type QPop = Bool
type QPushValid = Bool
type QPopValid = Bool

type QState = (QMem, QMem, QCursor)
type QInput = (QPush, QPop, QData)
type QOutput = (QPushValid, QPopValid, QData, Int)

-- Note: assumed that queue never overflows
queue :: HiddenClockResetEnable dom => Signal dom QInput -> Signal dom QOutput
queue input = output
    where 
        -- keeping in registers to avoid any combinational output
        output = bundle (pushValid, popValid, outData, waited)
        state = bundle (buffer, wait, cursor)
        buffer = register (repeat qNullData :: QMem) nextBufferSignal
        wait = register (repeat 0 :: QWait) nextWaitSignal
        cursor = register 0 nextCursorSignal
        (outData, waited) = unbundle (register (qNullData, -1) nextOutDataSignal)
        pushValid = register False nextPushValidSignal
        popValid = register False nextPopValidSignal

        -- Signal is an applicative functor
        -- <$> = alias for fmap - wraps normal function into a function on Signal
        -- <*> = apply applicative - it applies signal of function to a signal of a value

        nextBufferSignal = nextBuffer <$> buffer <*> bundle (input, cursor)
        nextWaitSignal = nextWait <$> wait <*> bundle (input, cursor)
        nextCursorSignal = nextCursor <$> cursor <*> bundle (input, buffer)
        nextOutDataSignal = nextOutData <$> bundle (input, cursor, buffer, wait)
        nextPushValidSignal = nextPushValid <$> bundle (input, cursor, buffer)
        nextPopValidSignal = nextPopValid <$> bundle (input, cursor)
        
        nextBuffer :: QMem -> (QInput, QCursor) -> QMem
        nextBuffer buf ((push, pop, qData), cur) = out
            where 
                out = case (push, pop) of
                    (True, _) -> if cur /= length buf then qData +>> buf else buf
                    (_, _) -> buf

        nextWait :: QWait -> (QInput, QCursor) -> QWait
        nextWait curWait curInptCursor = map updateWait (prepForUpdate curWait curInptCursor)
            where
                updateWait :: (Int, Int, Int, QCursor, QPush, QPop) -> Int
                updateWait (valToLeft, val, index, cur, push, pop) = out
                    where 
                        out = case (push, pop) of
                            (False, False) -> if index < cur then val + 1 else -1
                            (True, False) -> if index == 0 then 0 
                                            else if index <= cur then valToLeft + 1 else -1
                            (False, True) -> if index < (cur - 1) then val + 1 else -1
                            (True, True) -> if index == 0 then 0
                                            else if index < cur then valToLeft + 1 else -1

                prepForUpdate :: QWait -> (QInput, QCursor) -> Vec QMemSize (Int, Int, Int, QCursor, QPush, QPop)
                prepForUpdate curWait ((push, pop, _), cur) = out
                    where 
                        indices = iterateI (+1) 0 :: Vec QMemSize Int
                        shiftedRight = (0 :> Nil) ++ init curWait
                        out = zipWith (\(psh, pp, c) (i, valToLeft, val) -> (valToLeft, val, i, c, psh, pp))
                                (repeat (push, pop, cur))
                                (zip3 indices shiftedRight curWait)


        nextCursor :: QCursor -> (QInput, QMem) -> QCursor
        nextCursor cur ((push, pop, _), buf) = out
            where 
                out = case (push, pop) of
                    (True, False) -> if cur /= length buf then cur + 1 else cur
                    (False, True) -> if cur /= 0 then cur - 1 else 0
                    (True, True) -> if cur == 0 then cur + 1 else cur 
                    (_, _) -> cur

        nextOutData :: (QInput, QCursor, QMem, QWait) -> (QData, Int)
        nextOutData ((push, pop, _), cur, buf, _wait) = out
            where 
                out = case (push, pop) of
                    (_, True) -> if cur == 0 then (qNullData, -1) else (buf !! (cur - 1), (_wait !! (cur - 1)) + 1)
                    (_, _) -> (qNullData, -1)

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