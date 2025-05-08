module Spec where

import Clash.Prelude

---------------------------------------------------------------

-- input x : Int
-- 
-- output a := x + c.offset(by: -1).defaults(to: 0)
-- output b := x + a
-- output c := b + n.offset(by: -1).defaults(to: -1)
-- output d := c.offset(by: -1).defaults(to: 0)
-- output e := d + 1
-- output f := d + 1
-- output g := e + f
-- output h := g.offset(by: -1).defaults(to: 0) + l.hold(or: 0)
-- output i := h + 1
-- output j @1kHz := j.offset(by: -1).defaults(to: 0) + 1
-- output k := j + l.offset(by: -1).defaults(to: 0)
-- output l := k + 1
-- output m := i.offset(by: -1).defaults(to: 0) + n.offset(by: -1).defaults(to: 0)
-- output n := m + 1
-- 
-- 

---------------------------------------------------------------

-- Evaluation Order
-- j, x
-- k, a, d
-- l, b, f, e, m
-- h, c, g, n
-- i

-- Memory Window
-- window a = 2
-- window b = 1
-- window h = 1
-- window l = 1
-- window e = 1
-- window n = 1
-- window k = 2
-- window c = 1
-- window i = 1
-- window d = 2
-- window x = 1
-- window g = 1
-- window f = 1
-- window m = 1
-- window j = 2

-- Pipeline Visualization
-- j,x       |           |           | j,x       |           |           | j,x       |           |           | j,x      
-- ---------------------------------------------------------------------------------------------------------------------
--           | k,a,d     |           |           | k,a,d     |           |           | k,a,d     |           |          
-- ---------------------------------------------------------------------------------------------------------------------
--           |           | l,b,f,e,m |           |           | l,b,f,e,m |           |           | l,b,f,e,m |          
-- ---------------------------------------------------------------------------------------------------------------------
--           |           |           | h,c,g,n   |           |           | h,c,g,n   |           |           | h,c,g,n  
-- ---------------------------------------------------------------------------------------------------------------------
--           |           |           |           | i         |           |           | i         |           |          
-- ---------------------------------------------------------------------------------------------------------------------

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
-- output10 = k
-- output11 = l
-- output12 = m
-- output13 = n

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
type HasOutput10 = (Int, Bool)
type HasOutput11 = (Int, Bool)
type HasOutput12 = (Int, Bool)
type HasOutput13 = (Int, Bool)
type Outputs = (HasOutput0, HasOutput1, HasOutput2, HasOutput3, HasOutput4, HasOutput5, HasOutput6, HasOutput7, HasOutput8, HasOutput9, HasOutput10, HasOutput11, HasOutput12, HasOutput13)

type Pacings = (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)

type Event = (Inputs, Pacings)

nullEvent :: Event
nullEvent = ((0, False), (False, False, False, False, False, False, False, False, False, False, False, False, False, False))


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
        newEvent = hasInput0 .||. pacing9 .||. pacing10 .||. pacing11
        event = bundle (inputs, pacings)

        pacings = bundle (pacing0, pacing1, pacing2, pacing3, pacing4, pacing5, pacing6, pacing7, pacing8, pacing9, pacing10, pacing11, pacing12, pacing13)

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
        pacing9 = timer0Over
        pacing10 = timer0Over
        pacing11 = timer0Over
        pacing12 = hasInput0
        pacing13 = hasInput0


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
        (p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13) = unbundle pacings

        tagOut9 = genTag p9
        tagIn0 = genTag input0HasData
        tagOut10 = genTag p10
        tagOut0 = genTag p0
        tagOut3 = genTag p3
        tagOut11 = genTag p11
        tagOut1 = genTag p1
        tagOut5 = genTag p5
        tagOut4 = genTag p4
        tagOut12 = genTag p12
        tagOut7 = genTag p7
        tagOut2 = genTag p2
        tagOut6 = genTag p6
        tagOut13 = genTag p13
        tagOut8 = genTag p8

        -- tag generation takes 1 cycle so we need to delay the input data
        (input0Data, _) = unbundle (delay (0, False) input0)

        -- delayed tags to be used in different levels 
        tagsDefault = (invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag, invalidTag)
        curTags = bundle (tagIn0, tagOut0, tagOut1, tagOut2, tagOut3, tagOut4, tagOut5, tagOut6, tagOut7, tagOut8, tagOut9, tagOut10, tagOut11, tagOut12, tagOut13)
        curTagsLevel1 = delayFor d1 tagsDefault curTags
        curTagsLevel2 = delayFor d2 tagsDefault curTags
        curTagsLevel3 = delayFor d3 tagsDefault curTags
        curTagsLevel4 = delayFor d4 tagsDefault curTags
        curTagsLevel5 = delayFor d5 tagsDefault curTags

        enOut9 = delayFor d1 False p9
        enIn0 = delayFor d1 False input0HasData
        enOut10 = delayFor d2 False p10
        enOut0 = delayFor d2 False p0
        enOut3 = delayFor d2 False p3
        enOut11 = delayFor d3 False p11
        enOut1 = delayFor d3 False p1
        enOut5 = delayFor d3 False p5
        enOut4 = delayFor d3 False p4
        enOut12 = delayFor d3 False p12
        enOut7 = delayFor d4 False p7
        enOut2 = delayFor d4 False p2
        enOut6 = delayFor d4 False p6
        enOut13 = delayFor d4 False p13
        enOut8 = delayFor d5 False p8

        output0Aktv = delayFor d6 False p0
        output1Aktv = delayFor d6 False p1
        output2Aktv = delayFor d6 False p2
        output3Aktv = delayFor d6 False p3
        output4Aktv = delayFor d6 False p4
        output5Aktv = delayFor d6 False p5
        output6Aktv = delayFor d6 False p6
        output7Aktv = delayFor d6 False p7
        output8Aktv = delayFor d6 False p8
        output9Aktv = delayFor d6 False p9
        output10Aktv = delayFor d6 False p10
        output11Aktv = delayFor d6 False p11
        output12Aktv = delayFor d6 False p12
        output13Aktv = delayFor d6 False p13

        -- Evaluation of input windows: level 0
        input0Win = input0Window enIn0 tagIn0 input0Data

        -- Evaluation of output 0: level 1
        (out0Level1TagIn0, out0Level1TagOut0, _, out0Level1TagOut2, _, _, _, _, _, _, _, _, _, _, _) = unbundle curTagsLevel1
        out0 = outputStream0 enOut0 out0Level1TagOut0 out0Data0 out0Data1 
        out0Data0 = getMatchingTagFromNonVec <$> input0Win <*> out0Level1TagIn0 <*> (pure 0)
        out0Data1 = getOffsetFromNonVec <$> out2 <*> out0Level1TagOut2 <*> (pure 1) <*> out0Data1Dflt
        out0Data1Dflt = pure 0

        -- Evaluation of output 1: level 2
        (out1Level2TagIn0, out1Level2TagOut0, out1Level2TagOut1, _, _, _, _, _, _, _, _, _, _, _, _) = unbundle curTagsLevel2
        out1 = outputStream1 enOut1 out1Level2TagOut1 out1Data0 out1Data1 
        out1Data0 = getMatchingTagFromNonVec <$> input0Win <*> out1Level2TagIn0 <*> (pure 0)
        out1Data1 = getMatchingTag <$> out0 <*> out1Level2TagOut0 <*> (pure 0)

        -- Evaluation of output 2: level 3
        (_, _, out2Level3TagOut1, out2Level3TagOut2, _, _, _, _, _, _, _, _, _, _, out2Level3TagOut13) = unbundle curTagsLevel3
        out2 = outputStream2 enOut2 out2Level3TagOut2 out2Data0 out2Data1 
        out2Data0 = getMatchingTagFromNonVec <$> out1 <*> out2Level3TagOut1 <*> (pure 0)
        out2Data1 = getOffsetFromNonVec <$> out13 <*> out2Level3TagOut13 <*> (pure 1) <*> out2Data1Dflt
        out2Data1Dflt = pure -1

        -- Evaluation of output 3: level 1
        (_, _, _, out3Level1TagOut2, out3Level1TagOut3, _, _, _, _, _, _, _, _, _, _) = unbundle curTagsLevel1
        out3 = outputStream3 enOut3 out3Level1TagOut3 out3Data0 
        out3Data0 = getOffsetFromNonVec <$> out2 <*> out3Level1TagOut2 <*> (pure 1) <*> out3Data0Dflt
        out3Data0Dflt = pure 0

        -- Evaluation of output 4: level 2
        (_, _, _, _, out4Level2TagOut3, out4Level2TagOut4, _, _, _, _, _, _, _, _, _) = unbundle curTagsLevel2
        out4 = outputStream4 enOut4 out4Level2TagOut4 out4Data0 
        out4Data0 = getMatchingTag <$> out3 <*> out4Level2TagOut3 <*> (pure 0)

        -- Evaluation of output 5: level 2
        (_, _, _, _, out5Level2TagOut3, _, out5Level2TagOut5, _, _, _, _, _, _, _, _) = unbundle curTagsLevel2
        out5 = outputStream5 enOut5 out5Level2TagOut5 out5Data0 
        out5Data0 = getMatchingTag <$> out3 <*> out5Level2TagOut3 <*> (pure 0)

        -- Evaluation of output 6: level 3
        (_, _, _, _, _, out6Level3TagOut4, out6Level3TagOut5, out6Level3TagOut6, _, _, _, _, _, _, _) = unbundle curTagsLevel3
        out6 = outputStream6 enOut6 out6Level3TagOut6 out6Data0 out6Data1 
        out6Data0 = getMatchingTagFromNonVec <$> out4 <*> out6Level3TagOut4 <*> (pure 0)
        out6Data1 = getMatchingTagFromNonVec <$> out5 <*> out6Level3TagOut5 <*> (pure 0)

        -- Evaluation of output 7: level 3
        (_, _, _, _, _, _, _, out7Level3TagOut6, out7Level3TagOut7, _, _, _, out7Level3TagOut11, _, _) = unbundle curTagsLevel3
        out7 = outputStream7 enOut7 out7Level3TagOut7 out7Data0 out7Data1 
        out7Data0 = getOffsetFromNonVec <$> out6 <*> out7Level3TagOut6 <*> (pure 1) <*> out7Data0Dflt
        out7Data0Dflt = pure 0
        out7Data1 = getMatchingTagFromNonVec <$> out11 <*> out7Level3TagOut11 <*> out7Data1Dflt
        out7Data1Dflt = pure 0

        -- Evaluation of output 8: level 4
        (_, _, _, _, _, _, _, _, out8Level4TagOut7, out8Level4TagOut8, _, _, _, _, _) = unbundle curTagsLevel4
        out8 = outputStream8 enOut8 out8Level4TagOut8 out8Data0 
        out8Data0 = getMatchingTagFromNonVec <$> out7 <*> out8Level4TagOut7 <*> (pure 0)

        -- Evaluation of output 9: level 0
        out9 = outputStream9 enOut9 tagOut9 out9Data0 
        out9Data0 = getOffset <$> out9 <*> tagOut9 <*> (pure 1) <*> out9Data0Dflt
        out9Data0Dflt = pure 0

        -- Evaluation of output 10: level 1
        (_, _, _, _, _, _, _, _, _, _, out10Level1TagOut9, out10Level1TagOut10, out10Level1TagOut11, _, _) = unbundle curTagsLevel1
        out10 = outputStream10 enOut10 out10Level1TagOut10 out10Data0 out10Data1 
        out10Data0 = getMatchingTag <$> out9 <*> out10Level1TagOut9 <*> (pure 0)
        out10Data1 = getOffsetFromNonVec <$> out11 <*> out10Level1TagOut11 <*> (pure 1) <*> out10Data1Dflt
        out10Data1Dflt = pure 0

        -- Evaluation of output 11: level 2
        (_, _, _, _, _, _, _, _, _, _, _, out11Level2TagOut10, out11Level2TagOut11, _, _) = unbundle curTagsLevel2
        out11 = outputStream11 enOut11 out11Level2TagOut11 out11Data0 
        out11Data0 = getMatchingTag <$> out10 <*> out11Level2TagOut10 <*> (pure 0)

        -- Evaluation of output 12: level 2
        (_, _, _, _, _, _, _, _, _, out12Level2TagOut8, _, _, _, out12Level2TagOut12, out12Level2TagOut13) = unbundle curTagsLevel2
        out12 = outputStream12 enOut12 out12Level2TagOut12 out12Data0 out12Data1 
        out12Data0 = getOffsetFromNonVec <$> out8 <*> out12Level2TagOut8 <*> (pure 1) <*> out12Data0Dflt
        out12Data0Dflt = pure 0
        out12Data1 = getOffsetFromNonVec <$> out13 <*> out12Level2TagOut13 <*> (pure 1) <*> out12Data1Dflt
        out12Data1Dflt = pure 0

        -- Evaluation of output 13: level 3
        (_, _, _, _, _, _, _, _, _, _, _, _, _, out13Level3TagOut12, out13Level3TagOut13) = unbundle curTagsLevel3
        out13 = outputStream13 enOut13 out13Level3TagOut13 out13Data0 
        out13Data0 = getMatchingTagFromNonVec <$> out12 <*> out13Level3TagOut12 <*> (pure 0)

        -- Outputing all results: level 5
        (_, level5TagOut0, level5TagOut1, level5TagOut2, level5TagOut3, level5TagOut4, level5TagOut5, level5TagOut6, level5TagOut7, level5TagOut8, level5TagOut9, level5TagOut10, level5TagOut11, level5TagOut12, level5TagOut13) = unbundle curTagsLevel5
        output0 = bundle (output0Data, output0Aktv)
        (_, output0Data) = unbundle (getMatchingTag <$> out0 <*> level5TagOut0 <*> (pure 0))
        output1 = bundle (output1Data, output1Aktv)
        (_, output1Data) = unbundle out1
        output2 = bundle (output2Data, output2Aktv)
        (_, output2Data) = unbundle out2
        output3 = bundle (output3Data, output3Aktv)
        (_, output3Data) = unbundle (getMatchingTag <$> out3 <*> level5TagOut3 <*> (pure 0))
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
        output9 = bundle (output9Data, output9Aktv)
        (_, output9Data) = unbundle (getMatchingTag <$> out9 <*> level5TagOut9 <*> (pure 0))
        output10 = bundle (output10Data, output10Aktv)
        (_, output10Data) = unbundle (getMatchingTag <$> out10 <*> level5TagOut10 <*> (pure 0))
        output11 = bundle (output11Data, output11Aktv)
        (_, output11Data) = unbundle out11
        output12 = bundle (output12Data, output12Aktv)
        (_, output12Data) = unbundle out12
        output13 = bundle (output13Data, output13Aktv)
        (_, output13Data) = unbundle out13

        outputs = bundle (output0, output1, output2, output3, output4, output5, output6, output7, output8, output9, output10, output11, output12, output13)


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



input0Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
input0Window en tag val = result
    where result = register (invalidTag, 0) (mux en (bundle (tag, val)) result)



outputStream0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
outputStream0 en tag in0WithTag out2WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + out2
        (_, in0) = unbundle in0WithTag
        (_, out2) = unbundle out2WithTag


outputStream1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream1 en tag in0WithTag out0WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + out0
        (_, in0) = unbundle in0WithTag
        (_, out0) = unbundle out0WithTag


outputStream2 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream2 en tag out1WithTag out13WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out1 + out13
        (_, out1) = unbundle out1WithTag
        (_, out13) = unbundle out13WithTag


outputStream3 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
outputStream3 en tag out2WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out2
        (_, out2) = unbundle out2WithTag


outputStream4 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream4 en tag out3WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out3 + 1
        (_, out3) = unbundle out3WithTag


outputStream5 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream5 en tag out3WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out3 + 1
        (_, out3) = unbundle out3WithTag


outputStream6 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream6 en tag out4WithTag out5WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out4 + out5
        (_, out4) = unbundle out4WithTag
        (_, out5) = unbundle out5WithTag


outputStream7 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream7 en tag out6WithTag out11WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out6 + out11
        (_, out6) = unbundle out6WithTag
        (_, out11) = unbundle out11WithTag


outputStream8 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream8 en tag out7WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out7 + 1
        (_, out7) = unbundle out7WithTag


outputStream9 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
outputStream9 en tag out9WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out9 + 1
        (_, out9) = unbundle out9WithTag


outputStream10 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
outputStream10 en tag out9WithTag out11WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out9 + out11
        (_, out9) = unbundle out9WithTag
        (_, out11) = unbundle out11WithTag


outputStream11 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream11 en tag out10WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out10 + 1
        (_, out10) = unbundle out10WithTag


outputStream12 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream12 en tag out8WithTag out13WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out8 + out13
        (_, out8) = unbundle out8WithTag
        (_, out13) = unbundle out13WithTag


outputStream13 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream13 en tag out12WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out12 + 1
        (_, out12) = unbundle out12WithTag







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
