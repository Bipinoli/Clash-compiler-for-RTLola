module Spec where

import Clash.Prelude

---------------------------------------------------------------

-- input x : Int
-- 
-- output a := x.offset(by: -2).defaults(to: 0) + g.offset(by: -1).defaults(to: 0) + c.offset(by: -1).defaults(to: 0)
-- output b := a + 1
-- output c := b + 1
-- output d := b + 1
-- output e := b + 1
-- output f := d.offset(by: -1).defaults(to: 0) + e.offset(by: -1).defaults(to: 0)
-- output g := f + g.offset(by: -1).defaults(to: 0) + h + i
-- output h := x + 1
-- output i := x.offset(by: -1).defaults(to: 0) + 1
-- 

---------------------------------------------------------------

-- Evaluation Order
-- x, f, i, a
-- h, b
-- g, e, d, c

-- Memory Window
-- window x = 2
-- window f = 1
-- window i = 1
-- window a = 1
-- window h = 1
-- window b = 1
-- window g = 1
-- window e = 1
-- window d = 1
-- window c = 1

-- Pipeline Visualization
-- x,f,i,a |         |         | x,f,i,a |         |         | x,f,i,a |         |         | x,f,i,a
-- -------------------------------------------------------------------------------------------------
--         | h,b     |         |         | h,b     |         |         | h,b     |         |        
-- -------------------------------------------------------------------------------------------------
--         |         | g,e,d,c |         |         | g,e,d,c |         |         | g,e,d,c |        
-- -------------------------------------------------------------------------------------------------

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
type Outputs = (HasOutput0, HasOutput1, HasOutput2, HasOutput3, HasOutput4, HasOutput5, HasOutput6, HasOutput7, HasOutput8)

type Pacings = (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)

type Event = (Inputs, Pacings)

nullEvent :: Event
nullEvent = ((0, False), (False, False, False, False, False, False, False, False, False))


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



hlc :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Bool, Event)
hlc inputs = out
    where 
        out = bundle (newEvent, event)
        newEvent = hasInput0 .||. pacing0 .||. pacing1 .||. pacing2 .||. pacing3 .||. pacing4 .||. pacing5 .||. pacing6 .||. pacing7 .||. pacing8
        event = bundle (inputs, pacings)

        pacings = bundle (pacing0, pacing1, pacing2, pacing3, pacing4, pacing5, pacing6, pacing7, pacing8)

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





---------------------------------------------------------------

type Tag = Unsigned 8
-- maxTag must be at least the size of the maximum window to avoid duplicate tags in the window
-- also to avoid having to do modulo operations maxTag must be at least as big as the largest offset
maxTag = 3 :: Tag
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
        (p0, p1, p2, p3, p4, p5, p6, p7, p8) = unbundle pacings

        tagIn0 = genTag input0HasData
        tagOut5 = genTag p5
        tagOut8 = genTag p8
        tagOut0 = genTag p0
        tagOut7 = genTag p7
        tagOut1 = genTag p1
        tagOut6 = genTag p6
        tagOut4 = genTag p4
        tagOut3 = genTag p3
        tagOut2 = genTag p2

        -- tag generation takes 1 cycle so we need to delay the input data
        (input0Data, _) = unbundle (delay (0, False) input0)

        -- delayed tags to be used in different levels 
        tagsDefault = (invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag)
        curTags = bundle (tagIn0, tagOut0, tagOut1, tagOut2, tagOut3, tagOut4, tagOut5, tagOut6, tagOut7, tagOut8)
        curTagsLevel1 = delayFor d1 tagsDefault curTags
        curTagsLevel2 = delayFor d2 tagsDefault curTags
        curTagsLevel3 = delayFor d3 tagsDefault curTags

        enIn0 = delayFor d1 False input0HasData
        enOut5 = delayFor d1 False p5
        enOut8 = delayFor d1 False p8
        enOut0 = delayFor d1 False p0
        enOut7 = delayFor d2 False p7
        enOut1 = delayFor d2 False p1
        enOut6 = delayFor d3 False p6
        enOut4 = delayFor d3 False p4
        enOut3 = delayFor d3 False p3
        enOut2 = delayFor d3 False p2

        output0Aktv = delayFor d4 False p0
        output1Aktv = delayFor d4 False p1
        output2Aktv = delayFor d4 False p2
        output3Aktv = delayFor d4 False p3
        output4Aktv = delayFor d4 False p4
        output5Aktv = delayFor d4 False p5
        output6Aktv = delayFor d4 False p6
        output7Aktv = delayFor d4 False p7
        output8Aktv = delayFor d4 False p8

        -- Evaluation of input windows: level 0
        input0Win = input0Window enIn0 tagIn0 input0Data

        -- Evaluation of output 0: level 0
        out0 = outputStream0 enOut0 tagOut0 out0Data0 out0Data1 out0Data2 
        out0Data0 = getOffset <$> input0Win <*> tagIn0 <*> (pure 2) <*> out0Data0Dflt
        out0Data0Dflt = pure 0
        out0Data1 = getOffsetFromNonVec <$> out2 <*> tagOut2 <*> (pure 1) <*> out0Data1Dflt
        out0Data1Dflt = pure 0
        out0Data2 = getOffsetFromNonVec <$> out6 <*> tagOut6 <*> (pure 1) <*> out0Data2Dflt
        out0Data2Dflt = pure 0

        -- Evaluation of output 1: level 1
        (_, out1Level1TagOut0, out1Level1TagOut1, _, _, _, _, _, _, _) = unbundle curTagsLevel1
        out1 = outputStream1 enOut1 out1Level1TagOut1 out1Data0 
        out1Data0 = getMatchingTagFromNonVec <$> out0 <*> out1Level1TagOut0 <*> (pure 0)

        -- Evaluation of output 2: level 2
        (_, _, out2Level2TagOut1, out2Level2TagOut2, _, _, _, _, _, _) = unbundle curTagsLevel2
        out2 = outputStream2 enOut2 out2Level2TagOut2 out2Data0 
        out2Data0 = getMatchingTagFromNonVec <$> out1 <*> out2Level2TagOut1 <*> (pure 0)

        -- Evaluation of output 3: level 2
        (_, _, out3Level2TagOut1, _, out3Level2TagOut3, _, _, _, _, _) = unbundle curTagsLevel2
        out3 = outputStream3 enOut3 out3Level2TagOut3 out3Data0 
        out3Data0 = getMatchingTagFromNonVec <$> out1 <*> out3Level2TagOut1 <*> (pure 0)

        -- Evaluation of output 4: level 2
        (_, _, out4Level2TagOut1, _, _, out4Level2TagOut4, _, _, _, _) = unbundle curTagsLevel2
        out4 = outputStream4 enOut4 out4Level2TagOut4 out4Data0 
        out4Data0 = getMatchingTagFromNonVec <$> out1 <*> out4Level2TagOut1 <*> (pure 0)

        -- Evaluation of output 5: level 0
        out5 = outputStream5 enOut5 tagOut5 out5Data0 out5Data1 
        out5Data0 = getOffsetFromNonVec <$> out3 <*> tagOut3 <*> (pure 1) <*> out5Data0Dflt
        out5Data0Dflt = pure 0
        out5Data1 = getOffsetFromNonVec <$> out4 <*> tagOut4 <*> (pure 1) <*> out5Data1Dflt
        out5Data1Dflt = pure 0

        -- Evaluation of output 6: level 2
        (_, _, _, _, _, _, out6Level2TagOut5, out6Level2TagOut6, out6Level2TagOut7, out6Level2TagOut8) = unbundle curTagsLevel2
        out6 = outputStream6 enOut6 out6Level2TagOut6 out6Data0 out6Data1 out6Data2 out6Data3 
        out6Data0 = getMatchingTagFromNonVec <$> out5 <*> out6Level2TagOut5 <*> (pure 0)
        out6Data1 = getOffsetFromNonVec <$> out6 <*> out6Level2TagOut6 <*> (pure 1) <*> out6Data1Dflt
        out6Data1Dflt = pure 0
        out6Data2 = getMatchingTagFromNonVec <$> out7 <*> out6Level2TagOut7 <*> (pure 0)
        out6Data3 = getMatchingTagFromNonVec <$> out8 <*> out6Level2TagOut8 <*> (pure 0)

        -- Evaluation of output 7: level 1
        (out7Level1TagIn0, _, _, _, _, _, _, _, out7Level1TagOut7, _) = unbundle curTagsLevel1
        out7 = outputStream7 enOut7 out7Level1TagOut7 out7Data0 
        out7Data0 = getMatchingTag <$> input0Win <*> out7Level1TagIn0 <*> (pure 0)

        -- Evaluation of output 8: level 0
        out8 = outputStream8 enOut8 tagOut8 out8Data0 
        out8Data0 = getOffset <$> input0Win <*> tagIn0 <*> (pure 1) <*> out8Data0Dflt
        out8Data0Dflt = pure 0

        -- Outputing all results: level 3
        (_, level3TagOut0, level3TagOut1, level3TagOut2, level3TagOut3, level3TagOut4, level3TagOut5, level3TagOut6, level3TagOut7, level3TagOut8) = unbundle curTagsLevel3
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
        output8 = bundle (output8Data, output8Aktv)
        (_, output8Data) = unbundle out8

        outputs = bundle (output0, output1, output2, output3, output4, output5, output6, output7, output8)


        genTag :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag
        genTag en = t
            where 
                t = register 1 (mux en next_t t)
                next_t = mux (t .==. (pure maxTag)) (pure 1) (t + 1)


pipelineReady :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
pipelineReady rst = toWait .==. pure 0 
    where 
        waitTime = pure 2 :: Signal dom Int
        toWait = register (0 :: Int) next
        next = mux rst waitTime (mux (toWait .>. pure 0) (toWait - 1) toWait)



input0Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Vec 2 (Tag, Int))
input0Window en tag val = result
    where result = register (repeat (invalidTag, 0)) (mux en ((<<+) <$> result <*> (bundle (tag, val))) result)



outputStream0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream0 en tag in0WithTag out2WithTag out6WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + out6 + out2
        (_, in0) = unbundle in0WithTag
        (_, out2) = unbundle out2WithTag
        (_, out6) = unbundle out6WithTag


outputStream1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream1 en tag out0WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out0 + 1
        (_, out0) = unbundle out0WithTag


outputStream2 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream2 en tag out1WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out1 + 1
        (_, out1) = unbundle out1WithTag


outputStream3 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream3 en tag out1WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out1 + 1
        (_, out1) = unbundle out1WithTag


outputStream4 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream4 en tag out1WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out1 + 1
        (_, out1) = unbundle out1WithTag


outputStream5 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream5 en tag out3WithTag out4WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out3 + out4
        (_, out3) = unbundle out3WithTag
        (_, out4) = unbundle out4WithTag


outputStream6 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream6 en tag out5WithTag out6WithTag out7WithTag out8WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out5 + out6 + out7 + out8
        (_, out5) = unbundle out5WithTag
        (_, out6) = unbundle out6WithTag
        (_, out7) = unbundle out7WithTag
        (_, out8) = unbundle out8WithTag


outputStream7 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream7 en tag in0WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + 1
        (_, in0) = unbundle in0WithTag


outputStream8 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream8 en tag in0WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + 1
        (_, in0) = unbundle in0WithTag







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
