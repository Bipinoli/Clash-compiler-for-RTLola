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

-- input0 = x
-- input1 = y
-- output0 = a
-- output1 = b
-- output2 = c
-- sw0 = sw(b,c)
-- sw1 = sw(y,c)

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

type Tag = Unsigned 8
-- maxTag must be at least the size of the sliding window to avoid duplicate tags in the window
maxTag = 8 :: Tag
invalidTag = maxTag + 1

getOffset :: KnownNat n => Vec n (Tag, a) -> Tag -> Tag -> a -> (Tag, a)
getOffset win tag offset dflt = out
    where 
        offsetTag = if tag > offset then tag - offset else tag - offset + maxTag
        out = case findIndex (\(t, _) -> t == offsetTag) win of
            Just i -> let (_, v) = win !! i in (tag, v)
            Nothing -> (tag, dflt) 

getMatchingTag :: KnownNat n => Vec n (Tag, a) -> Tag -> a -> (Tag, a)
getMatchingTag win tag dflt = out
    where 
        out = case findIndex (\(t, _) -> t == tag) win of
            Just i -> let (_, v) = win !! i in (tag, v)
            Nothing -> (tag, dflt)



llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom (Bool, Outputs)
llc event = bundle (toPop, outputs)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        toPop = pure True

        (inputs, slides, pacings) = unbundle poppedEvent
        (input0, input1) = unbundle inputs
        (input0Data, input0HasData) = unbundle input0
        (input1Data, input1HasData) = unbundle input1
        (slide0, slide1) = unbundle slides
        (p0, p1, p2) = unbundle pacings

        outputs = bundle (output0, output1, output2)

        tag = genTag (p0 .||. p1 .||. p2)

        in1Tag = tag
        out0Tag = tag
        in0Tag = tag
        sw1Tag = delay invalidTag tag
        out1Tag = delay invalidTag tag
        sw0Tag = delay invalidTag (delay invalidTag tag)
        out2Tag = delay invalidTag (delay invalidTag (delay invalidTag tag))

        enIn1 = input1HasData
        enOut0 = p0
        enIn0 = input0HasData
        enSw1 = delay False p2
        sw1DataPacing = delay False input1HasData
        enOut1 = delay False p1
        enSw0 = delay False (delay False p2)
        sw0DataPacing = delay False (delay False p1)
        enOut2 = delay False (delay False (delay False p2))

        outputPhaseTag = delay invalidTag (delay invalidTag (delay invalidTag (delay invalidTag tag)))
        output0Aktv = delay False (delay False (delay False (delay False p0)))
        output1Aktv = delay False (delay False (delay False (delay False p1)))
        output2Aktv = delay False (delay False (delay False (delay False p2)))

        output0 = bundle (output0Data, output0Aktv)
        (_, output0Data) = unbundle (getMatchingTag <$> out0 <*> outputPhaseTag <*> (pure 0))
        output1 = bundle (output1Data, output1Aktv)
        (_, output1Data) = unbundle (getMatchingTag <$> out1 <*> outputPhaseTag <*> (pure 0))
        output2 = bundle (output2Data, output2Aktv)
        (_, output2Data) = unbundle out2

        input0Win = input0Window enIn0 (bundle (in0Tag, input0Data))
        input1Win = input1Window enIn1 (bundle (in1Tag, input1Data))

        out0 = outputStream0 enOut0 out0Data0 out0Data1 
        out0Data0 = getOffset <$> input0Win <*> out0Tag <*> (pure 2) <*> (pure 10)
        out0Data1 = getOffset <$> out1 <*> out0Tag <*> (pure 3) <*> (pure 20)

        out1 = outputStream1 enOut1 out1Data0 out1Data1 
        out1Data0 = input1Win
        out1Data1 = getMatchingTag <$> out0 <*> out1Tag <*> (pure 0)

        out2 = outputStream2 enOut2 out2Data0 out2Data1 out2Data2 
        out2Data0 = getMatchingTag <$> sw1 <*> out2Tag <*> (pure (repeat 0))
        out2Data1 = getMatchingTag <$> out0 <*> out2Tag <*> (pure 100)
        out2Data2 = sw0

        sw0 = slidingWindow0 enSw0 slide0 (bundle (sw0Tag, sw0Data))
        sw0Data = bundle (sw0DataVal, sw0DataPacing)
        (_, sw0DataVal) = unbundle (getMatchingTag <$> out1 <*> sw0Tag <*> (pure 0))

        sw1 = slidingWindow1 enSw1 slide1 (bundle (sw1Tag, sw1Data))
        sw1Data = bundle (sw1DataVal, sw1DataPacing)
        (_, sw1DataVal) = unbundle input1Win


        genTag :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag
        genTag en = t
            where 
                t = register 1 (mux en next_t t)
                next_t = mux (t .==. (pure maxTag)) (pure 1) (t + 1)





input0Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
input0Window en td = result
    where result = register (repeat (invalidTag, 0)) (mux en ((<<+) <$> result <*> td) result)


input1Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
input1Window en td = result
    where result = register (invalidTag, 0) (mux en td result)



outputStream0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Vec 3 (Tag, Int))
outputStream0 en in0WithTag out1WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> next <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + out1
        (tag, in0) = unbundle in0WithTag
        (_, out1) = unbundle out1WithTag


outputStream1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
outputStream1 en in1WithTag out0WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> next <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out0 + in1
        (tag, in1) = unbundle in1WithTag
        (_, out0) = unbundle out0WithTag


outputStream2 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, (Vec 3 Int)) -> Signal dom (Tag, Int) -> Signal dom (Tag, (Vec 4 Int)) -> Signal dom (Tag, Int)
outputStream2 en sw1WithTag out0WithTag sw0WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out0 + (merge0 <$> sw0) + (merge1 <$> sw1)
        (tag, sw1) = unbundle sw1WithTag
        (_, out0) = unbundle out0WithTag
        (_, sw0) = unbundle sw0WithTag
        merge1 :: Vec 3 Int -> Int
        merge1 win = fold windowBucketFunc1 win
        merge0 :: Vec 4 Int -> Int
        merge0 win = fold windowBucketFunc0 win



windowBucketFunc0 :: Int -> Int -> Int
windowBucketFunc0 acc item = acc + item

windowBucketFunc1 :: Int -> Int -> Int
windowBucketFunc1 acc item = acc + item


slidingWindow0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom (Tag, (Int, Bool)) -> Signal dom (Tag, (Vec 4 Int)) 
slidingWindow0 en slide hasInputWithTag = window
    where
        window = register (invalidTag, dflt) (mux en next window)
        next = bundle (tag, nextWindow <$> (snd <$> window) <*> slide <*> hasInput)
        dflt = repeat 0 :: Vec 4 Int
        (tag, hasInput) = unbundle hasInputWithTag

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


slidingWindow1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom (Tag, (Int, Bool)) -> Signal dom (Vec 2 (Tag, (Vec 3 Int))) 
slidingWindow1 en slide hasInputWithTag = window
    where
        window = register (repeat (invalidTag, dflt)) (mux en next window)
        next = (<<+) <$> next <*> bundle (tag, nextVal)
        nextVal = nextWindow <$> (snd <$> (last <$> window)) <*> slide <*> hasInput
        dflt = repeat 0 :: Vec 3 Int
        (tag, hasInput) = unbundle hasInputWithTag

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


monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom Outputs
monitor inputs = outputs
    where 
        (newEvent, event) = unbundle (hlc inputs)

        (qPushValid, qPopValid, qPopData) = unbundle (queue (bundle (qPush, qPop, qInptData)))
        qPush = newEvent
        qInptData = event

        (toPop, outputs) = unbundle (llc (bundle (qPopValid, qPopData)))
        qPop = toPop


---------------------------------------------------------------

topEntity :: Clock System -> Reset System -> Enable System -> 
    Signal System Inputs -> Signal System Outputs
topEntity clk rst en inputs = exposeClockResetEnable (monitor inputs) clk rst en
