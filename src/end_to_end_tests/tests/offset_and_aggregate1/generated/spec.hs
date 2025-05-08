module Spec where

import Clash.Prelude

---------------------------------------------------------------

-- input x : Int
-- input y : Int
-- 
-- output a := x + b.offset(by: -3).defaults(to: 2)
-- output b := a + y
-- output c @1kHz := a.hold(or: 1) + b.aggregate(over: 0.003s, using: sum)

---------------------------------------------------------------

-- Evaluation Order
-- x, y
-- a
-- b
-- sw(b,c)
-- c

-- Memory Window
-- window x = 1
-- window y = 2
-- window a = 3
-- window b = 3
-- window sw(b,c) = 1
-- window c = 1

-- Pipeline Visualization
-- x,y     | x,y     | x,y     | x,y     | x,y     | x,y     | x,y     | x,y     | x,y     | x,y    
-- -------------------------------------------------------------------------------------------------
--         | a       | a       | a       | a       | a       | a       | a       | a       | a      
-- -------------------------------------------------------------------------------------------------
--         |         | b       | b       | b       | b       | b       | b       | b       | b      
-- -------------------------------------------------------------------------------------------------
--         |         |         | sw(b,c) | sw(b,c) | sw(b,c) | sw(b,c) | sw(b,c) | sw(b,c) | sw(b,c)
-- -------------------------------------------------------------------------------------------------
--         |         |         |         | c       | c       | c       | c       | c       | c      
-- -------------------------------------------------------------------------------------------------

-- input0 = x
-- input1 = y
-- output0 = a
-- output1 = b
-- output2 = c
-- sw0 = sw(b,c)

---------------------------------------------------------------


type HasInput0 = (Int, Bool)
type HasInput1 = (Int, Bool)
type Inputs = (HasInput0, HasInput1)

type HasOutput0 = (Int, Bool)
type HasOutput1 = (Int, Bool)
type HasOutput2 = (Int, Bool)
type Outputs = (HasOutput0, HasOutput1, HasOutput2)

type Pacings = (Bool, Bool, Bool)
type Slides = Bool

type Event = (Inputs, Slides, Pacings)

nullEvent :: Event
nullEvent = (((0, False), (0, False)), False, (False, False, False))


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
        newEvent = hasInput0 .||. hasInput1 .||. pacing0 .||. pacing1 .||. pacing2 .||. slide0
        event = bundle (inputs, slides, pacings)

        slides = slide0
        pacings = bundle (pacing0, pacing1, pacing2)

        (input0, input1) = unbundle inputs
        (_, hasInput0) = unbundle input0
        (_, hasInput1) = unbundle input1

        pacing0 = hasInput0 .&&. hasInput1
        pacing1 = hasInput0 .&&. hasInput1
        pacing2 = timer0Over

        slide0 = timer0Over

        timer0Over = timer0 .>=. period0InNs
        timer0 = timer timer0Over
        period0InNs = 1000000

        timer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int
        timer reset = register 0 (mux reset (pure deltaTime) nextTime)
            where 
                nextTime = timer reset + pure deltaTime
                deltaTime = systemClockPeriodNs


---------------------------------------------------------------

type Tag = Unsigned 8
-- maxTag must be at least the size of the maximum window to avoid duplicate tags in the window
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

getMatchingTagFromNonVec :: (Tag, a) -> Tag -> a -> (Tag, a)
getMatchingTagFromNonVec (tag, dta) tagToMatch dflt = if tag == tagToMatch then (tag, dta) else (tagToMatch, dflt)

earlierTag :: Tag -> Tag -> Tag
earlierTag curTag cyclesBefore = if curTag > cyclesBefore then curTag - cyclesBefore else curTag - cyclesBefore + maxTag

delayFor :: forall dom n a . (HiddenClockResetEnable dom, KnownNat n, NFDataX a) => SNat n -> a -> Signal dom a -> Signal dom a
delayFor n initVal sig = last delayedVec
    where
      delayedVec :: Vec (n + 1) (Signal dom a)
      delayedVec = iterateI (delay initVal) sig
     

llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom ((Bool, Outputs), ((Bool, Bool, Bool), Bool))
llc event = bundle (bundle (toPop, outputs), debugSignals)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        toPop = pure True

        (inputs, slides, pacings) = unbundle poppedEvent
        (input0, input1) = unbundle inputs
        (_, input0HasData) = unbundle input0
        (_, input1HasData) = unbundle input1
        slide0 = slides
        (p0, p1, p2) = unbundle pacings

        tagIn0 = genTag input0HasData
        tagIn1 = genTag input1HasData
        tagOut0 = genTag p0
        tagOut1 = genTag p1
        tagSw0 = genTag p0
        tagOut2 = genTag p2

        -- tag generation takes 1 cycle so we need to delay the input data
        (input0Data, _) = unbundle (delay (0, False) input0)
        (input1Data, _) = unbundle (delay (0, False) input1)

        -- delayed tags to be used in different levels 
        tagsDefault = (invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag)
        curTags = bundle (tagIn0, tagIn1, tagOut0, tagOut1, tagOut2, tagSw0)
        curTagsLevel1 = delayFor d1 tagsDefault curTags
        curTagsLevel2 = delayFor d2 tagsDefault curTags
        curTagsLevel3 = delayFor d3 tagsDefault curTags
        curTagsLevel4 = delayFor d4 tagsDefault curTags
        curTagsLevel5 = delayFor d5 tagsDefault curTags

        enIn0 = delayFor d1 False input0HasData
        enIn1 = delayFor d1 False input1HasData
        enOut0 = delayFor d2 False p0
        enOut1 = delayFor d3 False p1
        enSw0 = delayFor d4 False (slide0 .||. p1)
        sld0 = delayFor d4 False slide0
        sw0DataPacing = delayFor d4 False p1
        enOut2 = delayFor d5 False p2

        output0Aktv = delayFor d6 False p0
        output1Aktv = delayFor d6 False p1
        output2Aktv = delayFor d6 False p2

        -- Evaluation of input windows: level 0
        input0Win = input0Window enIn0 tagIn0 input0Data
        input1Win = input1Window enIn1 tagIn1 input1Data

        -- Evaluation of output 0: level 1
        (out0Level1TagIn0, _, out0Level1TagOut0, out0Level1TagOut1, _, _) = unbundle curTagsLevel1
        out0 = outputStream0 enOut0 out0Level1TagOut0 out0Data0 out0Data1 
        out0Data0 = getMatchingTagFromNonVec <$> input0Win <*> out0Level1TagIn0 <*> (pure 0)
        out0Data1 = getOffset <$> out1 <*> out0Level1TagOut1 <*> (pure 3) <*> out0Data1Dflt
        out0Data1Dflt = pure 2

        -- Evaluation of output 1: level 2
        (_, out1Level2TagIn1, out1Level2TagOut0, out1Level2TagOut1, _, _) = unbundle curTagsLevel2
        out1 = outputStream1 enOut1 out1Level2TagOut1 out1Data0 out1Data1 
        out1Data0 = getMatchingTag <$> input1Win <*> out1Level2TagIn1 <*> (pure 0)
        out1Data1 = getMatchingTag <$> out0 <*> out1Level2TagOut0 <*> (pure 0)

        -- Evaluation of output 2: level 4
        (_, _, out2Level4TagOut0, _, out2Level4TagOut2, out2Level4TagSw0) = unbundle curTagsLevel4
        out2 = outputStream2 enOut2 out2Level4TagOut2 out2Data0 out2Data1 
        out2Data0 = getMatchingTag <$> out0 <*> out2Level4TagOut0 <*> out2Data0Dflt
        out2Data0Dflt = pure 1
        out2Data1 = sw0

        -- Evaluation of sliding window 0: level 3
        (_, _, _, sw0Level3TagOut1, _, sw0Level3TagSw0) = unbundle curTagsLevel3
        sw0 = slidingWindow0 enSw0 sld0 (bundle (sw0Level3TagSw0, sw0Data))
        sw0Data = bundle (sw0DataVal, sw0DataPacing)
        (_, sw0DataVal) = unbundle (getMatchingTag <$> out1 <*> sw0Level3TagOut1 <*> (pure 0))

        -- Outputing all results: level 5
        (_, _, level5TagOut0, level5TagOut1, level5TagOut2, _) = unbundle curTagsLevel5
        output0 = bundle (output0Data, output0Aktv)
        (_, output0Data) = unbundle (getMatchingTag <$> out0 <*> level5TagOut0 <*> (pure 0))
        output1 = bundle (output1Data, output1Aktv)
        (_, output1Data) = unbundle (getMatchingTag <$> out1 <*> level5TagOut1 <*> (pure 0))
        output2 = bundle (output2Data, output2Aktv)
        (_, output2Data) = unbundle out2

        outputs = bundle (output0, output1, output2)

        debugSignals = bundle (pacings, slides)

        genTag :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag
        genTag en = t
            where 
                t = register 1 (mux en next_t t)
                next_t = mux (t .==. (pure maxTag)) (pure 1) (t + 1)





input0Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
input0Window en tag val = result
    where result = register (invalidTag, 0) (mux en (bundle (tag, val)) result)


input1Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Vec 2 (Tag, Int))
input1Window en tag val = result
    where result = register (repeat (invalidTag, 0)) (mux en ((<<+) <$> result <*> (bundle (tag, val))) result)



outputStream0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Vec 3 (Tag, Int))
outputStream0 en tag in0WithTag out1WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + out1
        (_, in0) = unbundle in0WithTag
        (_, out1) = unbundle out1WithTag


outputStream1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Vec 3 (Tag, Int))
outputStream1 en tag in1WithTag out0WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out0 + in1
        (_, in1) = unbundle in1WithTag
        (_, out0) = unbundle out0WithTag


outputStream2 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, (Vec 4 Int)) -> Signal dom (Tag, Int)
outputStream2 en tag out0WithTag sw0WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out0 + (merge0 <$> sw0)
        (_, out0) = unbundle out0WithTag
        (_, sw0) = unbundle sw0WithTag
        merge0 :: Vec 4 Int -> Int
        merge0 win = fold windowBucketFunc0 (tail win)



windowBucketFunc0 :: Int -> Int -> Int
windowBucketFunc0 acc item = acc + item


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
                    (False, True) -> lastBucketUpdated
                    (True, False) -> 0 +>> win
                    (True, True) -> 0 +>> lastBucketUpdated
                lastBucketUpdated = replace 0 (windowBucketFunc0 (head win) dta) win




---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Outputs, (QPush, QPop, QPushValid, QPopValid, (Bool, Bool, Bool), Bool))
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
    Signal TestDomain Inputs -> Signal TestDomain (Outputs, (QPush, QPop, QPushValid, QPopValid, (Bool, Bool, Bool), Bool))
topEntity clk rst en inputs = exposeClockResetEnable (monitor inputs) clk rst en
