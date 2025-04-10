module Simple where

import Clash.Prelude

---------------------------------------------------------------

-- input x : Int
-- input y : Int
-- 
-- output a := x.offset(by: -2).defaults(to: 10) + b.offset(by: -3).defaults(to: 20)
-- output b := a + y
-- output c @1kHz := a.hold(or: 100) + b.aggregate(over: 0.003s, using: sum) + y.aggregate(over: 0.002s, using: sum)

---------------------------------------------------------------

-- Evaluation Order
-- y, a, x
-- sw(y,c), b
-- sw(b,c)
-- c

-- Memory Window
-- window y = 1
-- window a = 3
-- window x = 2
-- window sw(y,c) = 2
-- window b = 2
-- window sw(b,c) = 1
-- window c = 1

-- Pipeline Visualization
-- y,a,x     | y,a,x     | y,a,x     | y,a,x     | y,a,x     | y,a,x     | y,a,x     | y,a,x     | y,a,x     | y,a,x    
-- ---------------------------------------------------------------------------------------------------------------------
--           | sw(y,c),b | sw(y,c),b | sw(y,c),b | sw(y,c),b | sw(y,c),b | sw(y,c),b | sw(y,c),b | sw(y,c),b | sw(y,c),b
-- ---------------------------------------------------------------------------------------------------------------------
--           |           | sw(b,c)   | sw(b,c)   | sw(b,c)   | sw(b,c)   | sw(b,c)   | sw(b,c)   | sw(b,c)   | sw(b,c)  
-- ---------------------------------------------------------------------------------------------------------------------
--           |           |           | c         | c         | c         | c         | c         | c         | c        
-- ---------------------------------------------------------------------------------------------------------------------

---------------------------------------------------------------


type HasInput0 = (Int, Bool)
type HasInput1 = (Int, Bool)
type Inputs = (HasInput0, HasInput1)

type HasOutput0 = (Int, Bool)
type HasOutput1 = (Int, Bool)
type HasOutput2 = (Int, Bool)
type Outputs = (HasOutput0, HasOutput1, HasOutput2)

type Pacings = (Bool, Bool, Bool)
type Slides = (Bool, Bool)

type Event = (Inputs, Slides, Pacings)

nullEvent :: Event
nullEvent = (((0, False), (0, False)), (False, False), (False, False, False))


---------------------------------------------------------------

type QMemSize = 2

type QData = Event
type QMem = Vec QMemSize QData
type QCursor = Int
type QPush = Bool
type QPop = Bool
type QPushValid = Bool
type QPopValid = Bool

type QState = (QMem, QCursor)
type QInput = (QPush, QPop, QData)
type QOutput = (QPushValid, QPopValid, QData)

queue :: HiddenClockResetEnable dom => Signal dom QInput -> Signal dom QOutput
queue input = output
    where 
        -- keeping in registers to avoid any combinational output
        output = bundle (pushValid, popValid, outData)
        state = bundle (buffer, cursor)
        buffer = register (repeat nullEvent :: QMem) nextBufferSignal
        cursor = register 0 nextCursorSignal
        pushValid = register False nextPushValidSignal
        popValid = register False nextPopValidSignal
        outData = register nullEvent nextOutDataSignal

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
                    (_, True) -> if cur == 0 then nullEvent else buf !! (cur - 1)
                    (_, _) -> nullEvent

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


---------------------------------------------------------------

systemClockPeriodNs :: Int
systemClockPeriodNs = fromInteger (snatToInteger $ clockPeriod @System)


hlc :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Bool, Event)
hlc inputs = out
    where 
        out = bundle (newEvent, event)
        newEvent = slide0 .||. slide1 .||. pacing0 .||. pacing1 .||. pacing2
        event = bundle (inputs, slides, pacings)

        slides = bundle (slide0, slide1)
        pacings = bundle (pacing0, pacing1, pacing2)

        (input0, input1) = unbundle inputs
        (_, hasInput0) = unbundle input0
        (_, hasInput1) = unbundle input1

        pacing0 = hasInput0 .&&. hasInput1
        pacing1 = hasInput0 .&&. hasInput1
        pacing2 = timer0Over

        slide0 = timer2Over
        slide1 = timer1Over

        timer0Over = timer0 .>=. period0InNs
        timer0 = timer timer0Over
        period0InNs = 1000000
        timer1Over = timer1 .>=. period1InNs
        timer1 = timer timer1Over
        period1InNs = 2000000
        timer2Over = timer2 .>=. period2InNs
        timer2 = timer timer2Over
        period2InNs = 3000000

        timer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int
        timer reset = register 0 (mux reset (pure deltaTime) nextTime)
            where 
                nextTime = timer reset + pure deltaTime
                deltaTime = systemClockPeriodNs


---------------------------------------------------------------



input0Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom (Vec 2 Int, Vec 2 Bool)
input0Window en d = bundle (dataOut, validOut)
    where 
        dataOut = register (repeat 0) (mux en ((<<+) <$> dataOut <*> d) dataOut)
        validOut = register (repeat False) (mux en ((<<+) <$> validOut <*> pure True) validOut)


input1Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom Int
input1Window en d = out
    where out = register 0 (mux en d out)



outputStream0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Int, Bool) -> Signal dom (Int, Bool) -> Signal dom (Vec 3 Int)
outputStream0 en in0 out1 = result
    where
        result = register (repeat 0) (mux en next result)
        next = (<<+) <$> next <*> nextVal
        nextVal = (mux (snd <$> in0) (fst <$> in0) (pure 10)) + (mux (snd <$> out1) (fst <$> out1) (pure 20))


outputStream1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom Int -> Signal dom (Vec 2 Int)
outputStream1 en in1 out0 = result
    where
        result = register (repeat 0) (mux en next result)
        next = (<<+) <$> next <*> nextVal
        nextVal = out0 + in1


outputStream2 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Vec 3 Int) -> Signal dom (Int, Bool) -> Signal dom (Vec 4 Int) -> Signal dom Int
outputStream2 en sw1 out0 sw0 = result
    where
        result = register 0 (mux en next result)
        next = (mux (snd <$> out0) (fst <$> out0) (pure 100)) + (merge0 <$> sw0) + (merge1 <$> sw1)
        merge1 :: Vec 3 Int -> Int
        merge1 win = fold windowBucketFunc1 win
        merge0 :: Vec 4 Int -> Int
        merge0 win = fold windowBucketFunc0 win


windowBucketFunc0 :: Int -> Int -> Int
windowBucketFunc0 acc item = acc + item

windowBucketFunc1 :: Int -> Int -> Int
windowBucketFunc1 acc item = acc + item


slidingWindow0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom (Int, Bool) -> Signal dom (Vec 4 Int)
slidingWindow0 en slide hasInput = window
    where
        window = register dflt (mux en next window)
        next = nextWindow <$> window <*> slide <*> hasInput
        dflt = repeat 0 :: Vec 4 Int

        nextWindow :: Vec 4 Int -> Bool -> (Int, Bool) -> Vec 4 Int
        nextWindow win toSlide inpt = out
            where
                (dta, hasData) = inpt
                out = case (toSlide, hasData) of
                    (False, False) -> win
                    (False, True) -> updatedWin
                    (True, False) -> win <<+ 0
                updatedWin = replace lastIndx (windowBucketFunc0 (last win) dta) win
                lastIndx = length win - 1       


slidingWindow1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom (Int, Bool) -> Signal dom (Vec 2 (Vec 3 Int)) 
slidingWindow1 en slide hasInput = window
    where
        window = register (repeat dflt) (mux en next window)
        next = (<<+) <$> next <*> nextVal
        nextVal = nextWindow <$> (last <$> window) <*> slide <*> hasInput
        dflt = repeat 0 :: Vec 3 Int

        nextWindow :: Vec 3 Int -> Bool -> (Int, Bool) -> Vec 3 Int
        nextWindow win toSlide inpt = out
            where
                (dta, hasData) = inpt
                out = case (toSlide, hasData) of
                    (False, False) -> win
                    (False, True) -> updatedWin
                    (True, False) -> win <<+ 0
                updatedWin = replace lastIndx (windowBucketFunc1 (last win) dta) win
                lastIndx = length win - 1       




---------------------------------------------------------------