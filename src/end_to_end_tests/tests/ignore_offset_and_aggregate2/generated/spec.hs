module Spec where

import Clash.Prelude

---------------------------------------------------------------

-- input x : Int
-- input y : Int
-- 
-- output a := x.offset(by: -1).defaults(to: 5 + 5) +  1 + d.offset(by: -1).defaults(to: 11)
-- output b := x.offset(by: -2).defaults(to: 100) + 1
-- output c := a + b + x + y
-- output d := a * c 
-- 
-- output e @2kHz := e.offset(by: -1).defaults(to: 0) + 1
-- output f @1kHz := x.aggregate(over: 0.003s, using: sum) 
-- output g := e + f + d.hold(or: 100)
-- output h @1kHz := d.aggregate(over: 0.004s, using: sum) 
-- 

---------------------------------------------------------------

-- Evaluation Order
-- x, y, a, b, e
-- sw(x,f), c
-- f, d
-- sw(d,h), g
-- h

-- Memory Window
-- window x = 2
-- window y = 1
-- window a = 1
-- window b = 1
-- window e = 1
-- window sw(x,f) = 1
-- window c = 1
-- window f = 1
-- window d = 1
-- window sw(d,h) = 1
-- window g = 1
-- window h = 1

-- Pipeline Visualization
-- x,y,a,b,e |           |           | x,y,a,b,e |           |           | x,y,a,b,e |           |           | x,y,a,b,e
-- ---------------------------------------------------------------------------------------------------------------------
--           | sw(x,f),c |           |           | sw(x,f),c |           |           | sw(x,f),c |           |          
-- ---------------------------------------------------------------------------------------------------------------------
--           |           | f,d       |           |           | f,d       |           |           | f,d       |          
-- ---------------------------------------------------------------------------------------------------------------------
--           |           |           | sw(d,h),g |           |           | sw(d,h),g |           |           | sw(d,h),g
-- ---------------------------------------------------------------------------------------------------------------------
--           |           |           |           | h         |           |           | h         |           |          
-- ---------------------------------------------------------------------------------------------------------------------

-- input0 = x
-- input1 = y
-- output0 = a
-- output1 = b
-- output2 = c
-- output3 = d
-- output4 = e
-- output5 = f
-- output6 = g
-- output7 = h
-- sw0 = sw(x,f)
-- sw1 = sw(d,h)

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
type HasOutput7 = (Int, Bool)
type Outputs = (HasOutput0, HasOutput1, HasOutput2, HasOutput3, HasOutput4, HasOutput5, HasOutput6, HasOutput7)

type Pacings = (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
type Slides = (Bool, Bool)

type Event = (Inputs, Slides, Pacings)

nullEvent :: Event
nullEvent = (((0, False), (0, False)), (False, False), (False, False, False, False, False, False, False, False))


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
                    (True, True) -> qData +>> buf 
                    (True, False) -> if cur == length buf then buf else qData +>> buf
                    (False, _) -> buf

        nextCursor :: QCursor -> (QInput, QMem) -> QCursor
        nextCursor cur ((push, pop, _), buf) = out
            where 
                out = case (push, pop) of
                    (True, False) -> if cur == length buf then cur else cur + 1
                    (False, True) -> if cur == 0 then 0 else cur - 1
                    (_, _) -> cur

        nextOutData :: (QInput, QCursor, QMem) -> QData
        nextOutData ((push, pop, qData), cur, buf) = out
            where 
                out = case (push, pop) of
                    (True, True) -> if cur == 0 then qData else buf !! (cur - 1)
                    (False, True) -> if cur == 0 then nullEvent else buf !! (cur - 1)
                    (_, _) -> nullEvent

        nextPushValid :: (QInput, QCursor, QMem) -> QPush
        nextPushValid ((push, pop, _), cur, buf) = out
            where 
                out = case (push, pop) of
                    (True, True) -> True
                    (True, False) -> cur /= length buf
                    (False, _) -> False

        nextPopValid :: (QInput, QCursor) -> QPop
        nextPopValid ((push, pop, _), cur) = out
            where 
                out = case (push, pop) of
                    (True, True) -> True
                    (False, True) -> cur /= 0
                    (_, False) -> False


---------------------------------------------------------------

-- Clock domain with 2 microseconds period (500 kHz)
-- It has been arbitrarily chosen for both monitor and the verilog testbench simulation
createDomain vSystem{vName="TestDomain", vPeriod=2000} -- period in nanoseconds

systemClockPeriodNs :: Int
systemClockPeriodNs = fromInteger (snatToInteger $ clockPeriod @TestDomain)


hlc :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Bool, Event)
hlc inputs = out
    where 
        out = bundle (newEvent, event)
        newEvent = slide0 .||. slide1 .||. pacing0 .||. pacing1 .||. pacing2 .||. pacing3 .||. pacing4 .||. pacing5 .||. pacing6 .||. pacing7
        event = bundle (inputs, slides, pacings)

        slides = bundle (slide0, slide1)
        pacings = bundle (pacing0, pacing1, pacing2, pacing3, pacing4, pacing5, pacing6, pacing7)

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
        pacing7 = timer1Over

        slide0 = timer1Over
        slide1 = timer1Over

        timer0Over = timer0 .>=. period0InNs
        timer0 = timer timer0Over
        period0InNs = 500000
        timer1Over = timer1 .>=. period1InNs
        timer1 = timer timer1Over
        period1InNs = 1000000

        timer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int
        timer reset = register 0 (mux reset (pure deltaTime) nextTime)
            where 
                nextTime = timer reset + pure deltaTime
                deltaTime = systemClockPeriodNs


---------------------------------------------------------------

type Tag = Unsigned 8
-- maxTag must be at least the size of the sliding window to avoid duplicate tags in the window
-- also to avoid having to do modulo operations maxTag must be at least as big as the largest offset
maxTag = 5 :: Tag
invalidTag = maxTag + 1

getOffset :: KnownNat n => Vec n (Tag, a) -> Tag -> Tag -> a -> (Tag, a)
getOffset win tag offset dflt = out
    where 
        offsetTag = earlierTag tag offset
        out = case findIndex (\(t, _) -> t == offsetTag) win of
            Just i -> let (_, v) = win !! i in (tag, v)
            Nothing -> (tag, dflt) 

getMatchingTag :: KnownNat n => Vec n (Tag, a) -> Tag -> a -> (Tag, a)
getMatchingTag win tag dflt = out
    where 
        out = case findIndex (\(t, _) -> t == tag) win of
            Just i -> let (_, v) = win !! i in (tag, v)
            Nothing -> (tag, dflt)

getOffsetFromNonVec :: (Tag, a) -> Tag -> Tag -> a -> (Tag, a)
getOffsetFromNonVec (winTag, winData) tag offset dflt = out
    where 
        offsetTag = earlierTag tag offset
        out = if offsetTag == winTag then (tag, winData) else (tag, dflt)

earlierTag :: Tag -> Tag -> Tag
earlierTag curTag cyclesBefore = if curTag > cyclesBefore then curTag - cyclesBefore else curTag - cyclesBefore + maxTag
     

llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom ((Bool, Outputs), ((Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool), (Bool, Bool)))
llc event = bundle (bundle (toPop, outputs), debugSignals)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        isPipelineReady = pipelineReady startNewPipeline
        startNewPipeline = mux (isPipelineReady .&&. isValidEvent) (pure True) (pure False)
        toPop = isPipelineReady .&&. not <$> startNewPipeline

        (inputs, slides, pacings) = unbundle poppedEvent
        (input0, input1) = unbundle inputs
        (_, input0HasData) = unbundle input0
        (_, input1HasData) = unbundle input1
        (slide0, slide1) = unbundle slides
        (p0, p1, p2, p3, p4, p5, p6, p7) = unbundle pacings

        tagIn0 = genTag input0HasData
        tagIn1 = genTag input1HasData
        tagOut0 = genTag p0
        tagOut1 = genTag p1
        tagOut4 = genTag p4
        tagSw0 = genTag p0
        tagOut2 = genTag p2
        tagOut5 = genTag p5
        tagOut3 = genTag p3
        tagSw1 = genTag p1
        tagOut6 = genTag p6
        tagOut7 = genTag p7

        -- tag generation takes 1 cycle so we need to delay the input data
        (input0Data, _) = unbundle (delay (0, False) input0)
        (input1Data, _) = unbundle (delay (0, False) input1)

        -- delayed tags to be used in different levels 
        tagsDefault = (invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag)
        curTags = bundle (tagIn0, tagIn1, tagOut0, tagOut1, tagOut2, tagOut3, tagOut4, tagOut5, tagOut6, tagOut7, tagSw0, tagSw1)
        curTagsLevel1 = delay tagsDefault curTags
        curTagsLevel2 = delay tagsDefault (delay tagsDefault curTags)
        curTagsLevel3 = delay tagsDefault (delay tagsDefault (delay tagsDefault curTags))
        curTagsLevel4 = delay tagsDefault (delay tagsDefault (delay tagsDefault (delay tagsDefault curTags)))
        curTagsLevel5 = delay tagsDefault (delay tagsDefault (delay tagsDefault (delay tagsDefault (delay tagsDefault curTags))))

        enIn0 = delay False input0HasData
        enIn1 = delay False input1HasData
        enOut0 = delay False p0
        enOut1 = delay False p1
        enOut4 = delay False p4
        enSw0 = delay False (delay False slide0 .||. p5)
        sld0 = delay False (delay False slide0)
        sw0DataPacing = delay False (delay False input0HasData)
        enOut2 = delay False (delay False p2)
        enOut5 = delay False (delay False (delay False p5))
        enOut3 = delay False (delay False (delay False p3))
        enSw1 = delay False (delay False (delay False (delay False slide1 .||. p7)))
        sld1 = delay False (delay False (delay False (delay False slide1)))
        sw1DataPacing = delay False (delay False (delay False (delay False p3)))
        enOut6 = delay False (delay False (delay False (delay False p6)))
        enOut7 = delay False (delay False (delay False (delay False (delay False p7))))

        output0Aktv = delay False (delay False (delay False (delay False (delay False (delay False p0)))))
        output1Aktv = delay False (delay False (delay False (delay False (delay False (delay False p1)))))
        output2Aktv = delay False (delay False (delay False (delay False (delay False (delay False p2)))))
        output3Aktv = delay False (delay False (delay False (delay False (delay False (delay False p3)))))
        output4Aktv = delay False (delay False (delay False (delay False (delay False (delay False p4)))))
        output5Aktv = delay False (delay False (delay False (delay False (delay False (delay False p5)))))
        output6Aktv = delay False (delay False (delay False (delay False (delay False (delay False p6)))))
        output7Aktv = delay False (delay False (delay False (delay False (delay False (delay False p7)))))

        -- level 0
        input0Win = input0Window enIn0 (bundle (tagIn0, input0Data))
        input1Win = input1Window enIn1 (bundle (tagIn1, input1Data))

        -- level 0
        (level0TagIn0, _, _, _, _, _, _, _, _, _, _, _) = unbundle curTagsLevel0
        out0 = outputStream0 enOut0 out0Data0 out0Data1 
        out0Data0 = getOffset <$> input0Win <*> level0TagIn0 <*> (pure 1) <*> (pure 0)
        out0Data1 = getOffsetFromNonVec <$> out3 <*> level0TagOut3 <*> (pure 1) <*> (pure 11)

        -- level 0
        (level0TagIn0, _, _, _, _, _, _, _, _, _, _, _) = unbundle curTagsLevel0
        out1 = outputStream1 enOut1 out1Data0 
        out1Data0 = getOffset <$> input0Win <*> level0TagIn0 <*> (pure 2) <*> (pure 100)

        -- level 1
        (level1TagIn0, _, _, _, _, _, _, _, _, _, _, _) = unbundle curTagsLevel1
        out2 = outputStream2 enOut2 out2Data0 out2Data1 out2Data2 out2Data3 
        out2Data0 = getMatchingTag <$> input0Win <*> level1TagIn0 <*> (pure 0)
        out2Data1 = input1Win
        out2Data2 = out0
        out2Data3 = out1

        -- level 2
        (_, _, _, _, _, _, _, _, _, _, _, _) = unbundle curTagsLevel2
        out3 = outputStream3 enOut3 out3Data0 out3Data1 
        out3Data0 = out0
        out3Data1 = out2

        -- level 0
        (_, _, _, _, _, _, _, _, _, _, _, _) = unbundle curTagsLevel0
        out4 = outputStream4 enOut4 out4Data0 
        out4Data0 = getOffsetFromNonVec <$> out4 <*> level0TagOut4 <*> (pure 1) <*> (pure 0)

        -- level 2
        (_, _, _, _, _, _, _, _, _, _, _, _) = unbundle curTagsLevel2
        out5 = outputStream5 enOut5 out5Data0 
        out5Data0 = sw0

        -- level 3
        (_, _, _, _, _, _, _, _, _, _, _, _) = unbundle curTagsLevel3
        out6 = outputStream6 enOut6 out6Data0 out6Data1 out6Data2 
        out6Data0 = out3
        out6Data1 = out4
        out6Data2 = out5

        -- level 4
        (_, _, _, _, _, _, _, _, _, _, _, _) = unbundle curTagsLevel4
        out7 = outputStream7 enOut7 out7Data0 
        out7Data0 = sw1

        -- level 1
        (level1TagIn0, _, _, _, _, _, _, _, _, _, level1TagSw0, _) = unbundle curTagsLevel1
        sw0 = slidingWindow0 enSw0 sld0 (bundle (level1TagSw0, sw0Data))
        sw0Data = bundle (sw0DataVal, sw0DataPacing)
        (_, sw0DataVal) = unbundle (getMatchingTag <$> input0Win <*> level1TagIn0 <*> (pure 0))

        -- level 3
        (_, _, _, _, _, level3TagOut3, _, _, _, _, _, level3TagSw1) = unbundle curTagsLevel3
        sw1 = slidingWindow1 enSw1 sld1 (bundle (level3TagSw1, sw1Data))
        sw1Data = bundle (sw1DataVal, sw1DataPacing)
        (_, sw1DataVal) = unbundle out3

        -- level 5
        (_, _, level5TagOut0, level5TagOut1, level5TagOut2, level5TagOut3, level5TagOut4, level5TagOut5, level5TagOut6, level5TagOut7, _, _) = unbundle curTagsLevel5
        output0 = bundle (output0Data, output0Aktv)
        (_, output0Data) = unbundle out0
        output1 = bundle (output1Data, output1Aktv)
        (_, output1Data) = unbundle out1
        output2 = bundle (output2Data, output2Aktv)
        (_, output2Data) = unbundle out2
        output3 = bundle (output3Data, output3Aktv)
        (_, output3Data) = unbundle out3
        output4 = bundle (output4Data, output4Aktv)
        (_, output4Data) = unbundle out4
        output5 = bundle (output5Data, output5Aktv)
        (_, output5Data) = unbundle out5
        output6 = bundle (output6Data, output6Aktv)
        (_, output6Data) = unbundle out6
        output7 = bundle (output7Data, output7Aktv)
        (_, output7Data) = unbundle out7

        outputs = bundle (output0, output1, output2, output3, output4, output5, output6, output7)

        debugSignals = bundle (pacings, slides)

        genTag :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag
        genTag en = t
            where 
                t = register 1 (mux en next_t t)
                next_t = mux (t .==. (pure maxTag)) (pure 1) (t + 1)


pipelineReady :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
pipelineReady rst = toWait .==. pure 0 
    where 
        waitTime = pure  :: Signal dom Int
        toWait = register (0 :: Int) next
        next = mux rst waitTime (mux (toWait .>. pure 0) (toWait - 1) toWait)



input0Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
input0Window en td = result
    where result = register (repeat (invalidTag, 0)) (mux en ((<<+) <$> result <*> td) result)


input1Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
input1Window en td = result
    where result = register (invalidTag, 0) (mux en td result)



outputStream0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream0 en in0WithTag out3WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + 1 + out3
        (tag, in0) = unbundle in0WithTag
        (_, out3) = unbundle out3WithTag


outputStream1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream1 en in0WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + 1
        (tag, in0) = unbundle in0WithTag


outputStream2 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream2 en in0WithTag in1WithTag out0WithTag out1WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out0 + out1 + in0 + in1
        (tag, in0) = unbundle in0WithTag
        (_, in1) = unbundle in1WithTag
        (_, out0) = unbundle out0WithTag
        (_, out1) = unbundle out1WithTag


outputStream3 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream3 en out0WithTag out2WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out0 * out2
        (tag, out0) = unbundle out0WithTag
        (_, out2) = unbundle out2WithTag


outputStream4 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream4 en out4WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out4 + 1
        (tag, out4) = unbundle out4WithTag


outputStream5 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, (Vec 3 Int)) -> Signal dom (Tag, Int)
outputStream5 en sw0WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = (merge0 <$> sw0)
        (tag, sw0) = unbundle sw0WithTag
        merge0 :: Vec 3 Int -> Int
        merge0 win = fold windowBucketFunc0 win


outputStream6 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream6 en out3WithTag out4WithTag out5WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out4 + out5 + out3
        (tag, out3) = unbundle out3WithTag
        (_, out4) = unbundle out4WithTag
        (_, out5) = unbundle out5WithTag


outputStream7 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, (Vec 4 Int)) -> Signal dom (Tag, Int)
outputStream7 en sw1WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = (merge1 <$> sw1)
        (tag, sw1) = unbundle sw1WithTag
        merge1 :: Vec 4 Int -> Int
        merge1 win = fold windowBucketFunc1 win



windowBucketFunc0 :: Int -> Int -> Int
windowBucketFunc0 acc item = acc + item

windowBucketFunc1 :: Int -> Int -> Int
windowBucketFunc1 acc item = acc + item


slidingWindow0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom (Tag, (Int, Bool)) -> Signal dom (Tag, (Vec 3 Int)) 
slidingWindow0 en slide hasInputWithTag = window
    where
        window = register (invalidTag, dflt) (mux en next window)
        next = bundle (tag, nextWindow <$> (snd <$> window) <*> slide <*> hasInput)
        dflt = repeat 0 :: Vec 3 Int
        (tag, hasInput) = unbundle hasInputWithTag

        nextWindow :: Vec 3 Int -> Bool -> (Int, Bool) -> Vec 3 Int
        nextWindow win toSlide inpt = out
            where
                (dta, hasData) = inpt
                out = case (toSlide, hasData) of
                    (False, False) -> win
                    (False, True) -> lastBucketUpdated
                    (True, False) -> win <<+ 0
                    (True, True) -> win <<+ dta
                lastBucketUpdated = replace lastIndx (windowBucketFunc0 (last win) dta) win
                lastIndx = length win - 1       


slidingWindow1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom (Tag, (Int, Bool)) -> Signal dom (Tag, (Vec 4 Int)) 
slidingWindow1 en slide hasInputWithTag = window
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
                    (False, True) -> lastBucketUpdated
                    (True, False) -> win <<+ 0
                    (True, True) -> win <<+ dta
                lastBucketUpdated = replace lastIndx (windowBucketFunc1 (last win) dta) win
                lastIndx = length win - 1       




---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Outputs, (QPush, QPop, QPushValid, QPopValid, (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool), (Bool, Bool)))
monitor inputs = bundle (outputs, debugSignals)
    where 
        (newEvent, event) = unbundle (hlc inputs)

        (qPushValid, qPopValid, qPopData) = unbundle (queue (bundle (qPush, qPop, qInptData)))
        qPush = newEvent
        qPop = toPop
        qInptData = event

        (llcOutput, llcDebug) = unbundle (llc (bundle (qPopValid, qPopData)))
        (toPop, outputs) = unbundle llcOutput

        (llcPacings, llcSlides) = unbundle llcDebug
        debugSignals = bundle (qPush, qPop, qPushValid, qPopValid, llcPacings, llcSlides)


---------------------------------------------------------------

topEntity :: Clock TestDomain -> Reset TestDomain -> Enable TestDomain -> 
    Signal TestDomain Inputs -> Signal TestDomain (Outputs, (QPush, QPop, QPushValid, QPopValid, (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool), (Bool, Bool)))
topEntity clk rst en inputs = exposeClockResetEnable (monitor inputs) clk rst en
