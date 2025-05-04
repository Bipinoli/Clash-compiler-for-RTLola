module SimpleMixed where

import Clash.Prelude


---------------------------------------------------------------


type HasInput0 = (Int, Bool)
type HasInput1 = (Int, Bool)
type Inputs = (HasInput0, HasInput1)

type HasOutput0 = (Int, Bool)
type HasOutput1 = (Int, Bool)
type HasOutput2 = (Int, Bool)
type HasOutput3 = (Int, Bool)
type HasOutput4 = (Int, Bool)
type HasOutput5 = (Int, Bool)
type HasOutput6 = (Int, Bool)
type Outputs = (HasOutput0, HasOutput1, HasOutput2, HasOutput3, HasOutput4, HasOutput5, HasOutput6)

type Pacings = (Bool, Bool, Bool, Bool, Bool, Bool, Bool)
type Slides = Bool

type Event = (Inputs, Slides, Pacings)

nullEvent :: Event
nullEvent = (((0, False), (0, False)), False, (False, False, False, False, False, False, False))


---------------------------------------------------------------

type QMemSize = 4

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
        newEvent = slide0 .||. pacing0 .||. pacing1 .||. pacing2 .||. pacing3 .||. pacing4 .||. pacing5 .||. pacing6
        event = bundle (inputs, slides, pacings)

        slides = slide0
        pacings = bundle (pacing0, pacing1, pacing2, pacing3, pacing4, pacing5, pacing6)

        (input0, input1) = unbundle inputs
        (_, hasInput0) = unbundle input0
        (_, hasInput1) = unbundle input1

        pacing0 = hasInput0 .&&. hasInput1
        pacing1 = hasInput0
        pacing2 = hasInput0 .&&. hasInput1
        pacing3 = hasInput0 .&&. hasInput1
        pacing4 = timer0Over
        pacing5 = timer1Over
        pacing6 = timer1Over

        slide0 = timer2Over

        timer0Over = timer0 .>=. period0InNs
        timer0 = timer timer0Over
        period0InNs = 500000
        timer1Over = timer1 .>=. period1InNs
        timer1 = timer timer1Over
        period1InNs = 1000000
        timer2Over = timer2 .>=. period2InNs
        timer2 = timer timer2Over
        period2InNs = 3000000

        timer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int
        timer reset = register 0 (mux reset (pure deltaTime) nextTime)
            where 
                nextTime = timer reset + pure deltaTime
                deltaTime = systemClockPeriodNs


---------------------------------------------------------------

pipelineReady :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
pipelineReady rst = toWait .==. pure 0 
    where 
        waitTime = pure 2 :: Signal dom Int
        toWait = register (0 :: Int) next
        next = mux rst waitTime (mux (toWait .>. pure 0) (toWait - 1) toWait)


input0Win :: HiddenClockResetEnable dom => Signal dom Bool -> 


stream0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom Int -> Signal dom Int
stream0 en d0 d1 = out
    where
        out = register 0 (mux en next out)
        next = d0 + 1 + d1


stream1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom Int
stream1 en d0 = out
    where
        out = register 0 (mux en next out)
        next = d0 + 1


stream2 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom Int -> Signal dom Int -> Signal dom Int -> Signal dom Int
stream2 en d0 d1 d2 d3 = out
    where
        out = register 0 (mux en next out)
        next = d0 + d1 + d2 + d3


stream3 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom Int -> Signal dom Int
stream3 en d0 d1 = out
    where
        out = register 0 (mux en next out)
        next = d0 * d1


stream4 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom Int
stream4 en d0 = out
    where
        out = register 0 (mux en next out)
        next = d0 + 1


stream5 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Vec 4 Int) -> Signal dom Int
stream5 en sw = out
    where
        out = register 0 (mux en next out)
        next = merge <$> sw
        merge :: Vec 4 Int -> Int
        merge win = fold windowBucketFunc0 win


stream6 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom Int -> Signal dom Int -> Signal dom Int
stream6 en d0 d1 d2 = out
    where
        out = register 0 (mux en next out)
        next = d0 + d1 + d2


windowBucketFunc0 :: Int -> Int -> Int
windowBucketFunc0 acc item = acc + item


slidingWindow0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom (Int, Bool) -> Signal dom (Vec 4 Int)
slidingWindow0 en slide hasInput = window
    where
        window = register dflt (mux en next window)
        dflt = repeat 0 :: Vec 4 Int
        next = nextWindow <$> window <*> slide <*> hasInput

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




---------------------------------------------------------------

llc :: HiddenClockResetEnable dom => Signal dom Event -> Signal dom Outputs
llc event = outputs
    where
        (inputs, slides, pacings) = unbundle event

        -- level 0 : input windows


        -- level 1
        en0 = pacing0
        en1 = pacing1
        en4 = pacing4

        -- level 2
        en2 = delay False pacing2
        enSW0 = delay False pacing5
        
        -- level 3
        en3 = delay False (delay False pacing3)
        en5 = delay False enSW0

        -- level 4
        en6 = delay False (delay False (delay False pacing6))
        enSW1 = delay False (delay False (delay False pacing6))


        a = x(-1).defaults(5 + 5) + 1 + d(-1).(11)
        out0 = stream0 pacing0  

