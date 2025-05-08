module Spec where

import Clash.Prelude

---------------------------------------------------------------

-- input x : Int
-- output a := x + 1
-- output b := a + x + e.offset(by: -1).defaults(to: 0)
-- output c := b + 1
-- output d := c + 1
-- output e := d + 1
-- output f := c + 1
-- output g := f.offset(by: -2).defaults(to: 0) + h.offset(by: -4).defaults(to: 0) + j.offset(by: -2).defaults(to: 0)
-- output h := f + 1
-- output i := g + 1  
-- output j := i + 1
-- 

---------------------------------------------------------------

-- Evaluation Order
-- x, g
-- a, i
-- b, j
-- c
-- f, d
-- h, e

-- Memory Window
-- window x = 1
-- window g = 2
-- window f = 2
-- window d = 1
-- window e = 1
-- window h = 4
-- window i = 2
-- window b = 1
-- window c = 1
-- window j = 2
-- window a = 2

-- Pipeline Visualization
-- x,g |     |     |     | x,g |     |     |     | x,g |    
-- ---------------------------------------------------------
--     | a,i |     |     |     | a,i |     |     |     | a,i
-- ---------------------------------------------------------
--     |     | b,j |     |     |     | b,j |     |     |    
-- ---------------------------------------------------------
--     |     |     | c   |     |     |     | c   |     |    
-- ---------------------------------------------------------
--     |     |     |     | f,d |     |     |     | f,d |    
-- ---------------------------------------------------------
--     |     |     |     |     | h,e |     |     |     | h,e
-- ---------------------------------------------------------

-- input0 = x
-- output0 = a
-- output1 = b
-- output2 = c
-- output3 = d
-- output4 = e
-- output5 = f
-- output6 = g
-- output7 = h
-- output8 = i
-- output9 = j

---------------------------------------------------------------


type HasInput0 = (Int, Bool)
type Inputs = HasInput0

type HasOutput0 = (Int, Bool)
type HasOutput1 = (Int, Bool)
type HasOutput2 = (Int, Bool)
type HasOutput3 = (Int, Bool)
type HasOutput4 = (Int, Bool)
type HasOutput5 = (Int, Bool)
type HasOutput6 = (Int, Bool)
type HasOutput7 = (Int, Bool)
type HasOutput8 = (Int, Bool)
type HasOutput9 = (Int, Bool)
type Outputs = (HasOutput0, HasOutput1, HasOutput2, HasOutput3, HasOutput4, HasOutput5, HasOutput6, HasOutput7, HasOutput8, HasOutput9)

type Pacings = (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)

type Event = (Inputs, Pacings)

nullEvent :: Event
nullEvent = ((0, False), (False, False, False, False, False, False, False, False, False, False))


---------------------------------------------------------------

type QMemSize = 5

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



hlc :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Bool, Event)
hlc inputs = out
    where 
        out = bundle (newEvent, event)
        newEvent = hasInput0
        event = bundle (inputs, pacings)

        pacings = bundle (pacing0, pacing1, pacing2, pacing3, pacing4, pacing5, pacing6, pacing7, pacing8, pacing9)

        (_, hasInput0) = unbundle inputs

        pacing0 = hasInput0
        pacing1 = hasInput0
        pacing2 = hasInput0
        pacing3 = hasInput0
        pacing4 = hasInput0
        pacing5 = hasInput0
        pacing6 = hasInput0
        pacing7 = hasInput0
        pacing8 = hasInput0
        pacing9 = hasInput0





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
     

llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom (Bool, Outputs)
llc event = bundle (toPop, outputs)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        isPipelineReady = pipelineReady startNewPipeline
        startNewPipeline = mux (isPipelineReady .&&. isValidEvent) (pure True) (pure False)
        toPop = isPipelineReady .&&. not <$> startNewPipeline

        (inputs, pacings) = unbundle poppedEvent
        input0 = inputs
        (_, input0HasData) = unbundle input0
        (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9) = unbundle pacings

        tagIn0 = genTag input0HasData
        tagOut6 = genTag p6
        tagOut0 = genTag p0
        tagOut8 = genTag p8
        tagOut1 = genTag p1
        tagOut9 = genTag p9
        tagOut2 = genTag p2
        tagOut5 = genTag p5
        tagOut3 = genTag p3
        tagOut7 = genTag p7
        tagOut4 = genTag p4

        -- tag generation takes 1 cycle so we need to delay the input data
        (input0Data, _) = unbundle (delay (0, False) input0)

        -- delayed tags to be used in different levels 
        tagsDefault = (invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag)
        curTags = bundle (tagIn0, tagOut0, tagOut1, tagOut2, tagOut3, tagOut4, tagOut5, tagOut6, tagOut7, tagOut8, tagOut9)
        curTagsLevel1 = delayFor d1 tagsDefault curTags
        curTagsLevel2 = delayFor d2 tagsDefault curTags
        curTagsLevel3 = delayFor d3 tagsDefault curTags
        curTagsLevel4 = delayFor d4 tagsDefault curTags
        curTagsLevel5 = delayFor d5 tagsDefault curTags
        curTagsLevel6 = delayFor d6 tagsDefault curTags

        enIn0 = delayFor d1 False input0HasData
        enOut6 = delayFor d1 False p6
        enOut0 = delayFor d2 False p0
        enOut8 = delayFor d2 False p8
        enOut1 = delayFor d3 False p1
        enOut9 = delayFor d3 False p9
        enOut2 = delayFor d4 False p2
        enOut5 = delayFor d5 False p5
        enOut3 = delayFor d5 False p3
        enOut7 = delayFor d6 False p7
        enOut4 = delayFor d6 False p4

        output0Aktv = delayFor d7 False p0
        output1Aktv = delayFor d7 False p1
        output2Aktv = delayFor d7 False p2
        output3Aktv = delayFor d7 False p3
        output4Aktv = delayFor d7 False p4
        output5Aktv = delayFor d7 False p5
        output6Aktv = delayFor d7 False p6
        output7Aktv = delayFor d7 False p7
        output8Aktv = delayFor d7 False p8
        output9Aktv = delayFor d7 False p9

        -- Evaluation of input windows: level 0
        input0Win = input0Window enIn0 tagIn0 input0Data

        -- Evaluation of output 0: level 1
        (out0Level1TagIn0, out0Level1TagOut0, _, _, _, _, _, _, _, _, _) = unbundle curTagsLevel1
        out0 = outputStream0 enOut0 out0Level1TagOut0 out0Data0 
        out0Data0 = getMatchingTagFromNonVec <$> input0Win <*> out0Level1TagIn0 <*> (pure 0)

        -- Evaluation of output 1: level 2
        (out1Level2TagIn0, out1Level2TagOut0, out1Level2TagOut1, _, _, out1Level2TagOut4, _, _, _, _, _) = unbundle curTagsLevel2
        out1 = outputStream1 enOut1 out1Level2TagOut1 out1Data0 out1Data1 out1Data2 
        out1Data0 = getMatchingTagFromNonVec <$> input0Win <*> out1Level2TagIn0 <*> (pure 0)
        out1Data1 = getMatchingTag <$> out0 <*> out1Level2TagOut0 <*> (pure 0)
        out1Data2 = getOffsetFromNonVec <$> out4 <*> out1Level2TagOut4 <*> (pure 1) <*> out1Data2Dflt
        out1Data2Dflt = pure 0

        -- Evaluation of output 2: level 3
        (_, _, out2Level3TagOut1, out2Level3TagOut2, _, _, _, _, _, _, _) = unbundle curTagsLevel3
        out2 = outputStream2 enOut2 out2Level3TagOut2 out2Data0 
        out2Data0 = getMatchingTagFromNonVec <$> out1 <*> out2Level3TagOut1 <*> (pure 0)

        -- Evaluation of output 3: level 4
        (_, _, _, out3Level4TagOut2, out3Level4TagOut3, _, _, _, _, _, _) = unbundle curTagsLevel4
        out3 = outputStream3 enOut3 out3Level4TagOut3 out3Data0 
        out3Data0 = getMatchingTagFromNonVec <$> out2 <*> out3Level4TagOut2 <*> (pure 0)

        -- Evaluation of output 4: level 5
        (_, _, _, _, out4Level5TagOut3, out4Level5TagOut4, _, _, _, _, _) = unbundle curTagsLevel5
        out4 = outputStream4 enOut4 out4Level5TagOut4 out4Data0 
        out4Data0 = getMatchingTagFromNonVec <$> out3 <*> out4Level5TagOut3 <*> (pure 0)

        -- Evaluation of output 5: level 4
        (_, _, _, out5Level4TagOut2, _, _, out5Level4TagOut5, _, _, _, _) = unbundle curTagsLevel4
        out5 = outputStream5 enOut5 out5Level4TagOut5 out5Data0 
        out5Data0 = getMatchingTagFromNonVec <$> out2 <*> out5Level4TagOut2 <*> (pure 0)

        -- Evaluation of output 6: level 0
        out6 = outputStream6 enOut6 tagOut6 out6Data0 out6Data1 out6Data2 
        out6Data0 = getOffset <$> out5 <*> tagOut5 <*> (pure 2) <*> out6Data0Dflt
        out6Data0Dflt = pure 0
        out6Data1 = getOffset <$> out7 <*> tagOut7 <*> (pure 4) <*> out6Data1Dflt
        out6Data1Dflt = pure 0
        out6Data2 = getOffset <$> out9 <*> tagOut9 <*> (pure 2) <*> out6Data2Dflt
        out6Data2Dflt = pure 0

        -- Evaluation of output 7: level 5
        (_, _, _, _, _, _, out7Level5TagOut5, _, out7Level5TagOut7, _, _) = unbundle curTagsLevel5
        out7 = outputStream7 enOut7 out7Level5TagOut7 out7Data0 
        out7Data0 = getMatchingTag <$> out5 <*> out7Level5TagOut5 <*> (pure 0)

        -- Evaluation of output 8: level 1
        (_, _, _, _, _, _, _, out8Level1TagOut6, _, out8Level1TagOut8, _) = unbundle curTagsLevel1
        out8 = outputStream8 enOut8 out8Level1TagOut8 out8Data0 
        out8Data0 = getMatchingTag <$> out6 <*> out8Level1TagOut6 <*> (pure 0)

        -- Evaluation of output 9: level 2
        (_, _, _, _, _, _, _, _, _, out9Level2TagOut8, out9Level2TagOut9) = unbundle curTagsLevel2
        out9 = outputStream9 enOut9 out9Level2TagOut9 out9Data0 
        out9Data0 = getMatchingTag <$> out8 <*> out9Level2TagOut8 <*> (pure 0)

        -- Outputing all results: level 6
        (_, level6TagOut0, level6TagOut1, level6TagOut2, level6TagOut3, level6TagOut4, level6TagOut5, level6TagOut6, level6TagOut7, level6TagOut8, level6TagOut9) = unbundle curTagsLevel6
        output0 = bundle (output0Data, output0Aktv)
        (_, output0Data) = unbundle (getMatchingTag <$> out0 <*> level6TagOut0 <*> (pure 0))
        output1 = bundle (output1Data, output1Aktv)
        (_, output1Data) = unbundle out1
        output2 = bundle (output2Data, output2Aktv)
        (_, output2Data) = unbundle out2
        output3 = bundle (output3Data, output3Aktv)
        (_, output3Data) = unbundle out3
        output4 = bundle (output4Data, output4Aktv)
        (_, output4Data) = unbundle out4
        output5 = bundle (output5Data, output5Aktv)
        (_, output5Data) = unbundle (getMatchingTag <$> out5 <*> level6TagOut5 <*> (pure 0))
        output6 = bundle (output6Data, output6Aktv)
        (_, output6Data) = unbundle (getMatchingTag <$> out6 <*> level6TagOut6 <*> (pure 0))
        output7 = bundle (output7Data, output7Aktv)
        (_, output7Data) = unbundle (getMatchingTag <$> out7 <*> level6TagOut7 <*> (pure 0))
        output8 = bundle (output8Data, output8Aktv)
        (_, output8Data) = unbundle (getMatchingTag <$> out8 <*> level6TagOut8 <*> (pure 0))
        output9 = bundle (output9Data, output9Aktv)
        (_, output9Data) = unbundle (getMatchingTag <$> out9 <*> level6TagOut9 <*> (pure 0))

        outputs = bundle (output0, output1, output2, output3, output4, output5, output6, output7, output8, output9)


        genTag :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag
        genTag en = t
            where 
                t = register 1 (mux en next_t t)
                next_t = mux (t .==. (pure maxTag)) (pure 1) (t + 1)


pipelineReady :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
pipelineReady rst = toWait .==. pure 0 
    where 
        waitTime = pure 3 :: Signal dom Int
        toWait = register (0 :: Int) next
        next = mux rst waitTime (mux (toWait .>. pure 0) (toWait - 1) toWait)



input0Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
input0Window en tag val = result
    where result = register (invalidTag, 0) (mux en (bundle (tag, val)) result)



outputStream0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
outputStream0 en tag in0WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + 1
        (_, in0) = unbundle in0WithTag


outputStream1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream1 en tag in0WithTag out0WithTag out4WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out0 + in0 + out4
        (_, in0) = unbundle in0WithTag
        (_, out0) = unbundle out0WithTag
        (_, out4) = unbundle out4WithTag


outputStream2 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream2 en tag out1WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out1 + 1
        (_, out1) = unbundle out1WithTag


outputStream3 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream3 en tag out2WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out2 + 1
        (_, out2) = unbundle out2WithTag


outputStream4 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream4 en tag out3WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out3 + 1
        (_, out3) = unbundle out3WithTag


outputStream5 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
outputStream5 en tag out2WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out2 + 1
        (_, out2) = unbundle out2WithTag


outputStream6 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
outputStream6 en tag out5WithTag out7WithTag out9WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out5 + out7 + out9
        (_, out5) = unbundle out5WithTag
        (_, out7) = unbundle out7WithTag
        (_, out9) = unbundle out9WithTag


outputStream7 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Vec 4 (Tag, Int))
outputStream7 en tag out5WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out5 + 1
        (_, out5) = unbundle out5WithTag


outputStream8 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
outputStream8 en tag out6WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out6 + 1
        (_, out6) = unbundle out6WithTag


outputStream9 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
outputStream9 en tag out8WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out8 + 1
        (_, out8) = unbundle out8WithTag







---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom Outputs
monitor inputs = outputs
    where 
        (newEvent, event) = unbundle (hlc inputs)

        (qPushValid, qPopValid, qPopData) = unbundle (queue (bundle (qPush, qPop, qInptData)))
        qPush = newEvent
        qPop = toPop
        qInptData = event

        (toPop, outputs) = unbundle (llc (bundle (qPopValid, qPopData)))


---------------------------------------------------------------

topEntity :: Clock TestDomain -> Reset TestDomain -> Enable TestDomain -> 
    Signal TestDomain Inputs -> Signal TestDomain Outputs
topEntity clk rst en inputs = exposeClockResetEnable (monitor inputs) clk rst en
