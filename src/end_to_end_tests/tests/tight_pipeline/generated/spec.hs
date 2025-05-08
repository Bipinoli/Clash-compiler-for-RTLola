module Spec where

import Clash.Prelude

---------------------------------------------------------------

-- input x : Int
-- output a := x + 1
-- output b := x + 10
-- output c := a + 1
-- output d := b + c
-- 

---------------------------------------------------------------

-- Evaluation Order
-- x
-- a, b
-- c
-- d

-- Memory Window
-- window x = 1
-- window a = 3
-- window c = 2
-- window d = 1
-- window b = 3

-- Pipeline Visualization
-- x   | x   | x   | x   | x   | x   | x   | x   | x   | x  
-- ---------------------------------------------------------
--     | a,b | a,b | a,b | a,b | a,b | a,b | a,b | a,b | a,b
-- ---------------------------------------------------------
--     |     | c   | c   | c   | c   | c   | c   | c   | c  
-- ---------------------------------------------------------
--     |     |     | d   | d   | d   | d   | d   | d   | d  
-- ---------------------------------------------------------

-- input0 = x
-- output0 = a
-- output1 = b
-- output2 = c
-- output3 = d

---------------------------------------------------------------


type HasInput0 = (Int, Bool)
type Inputs = HasInput0

type HasOutput0 = (Int, Bool)
type HasOutput1 = (Int, Bool)
type HasOutput2 = (Int, Bool)
type HasOutput3 = (Int, Bool)
type Outputs = (HasOutput0, HasOutput1, HasOutput2, HasOutput3)

type Pacings = (Bool, Bool, Bool, Bool)

type Event = (Inputs, Pacings)

nullEvent :: Event
nullEvent = ((0, False), (False, False, False, False))


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



hlc :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Bool, Event)
hlc inputs = out
    where 
        out = bundle (newEvent, event)
        newEvent = hasInput0 .||. pacing0 .||. pacing1 .||. pacing2 .||. pacing3
        event = bundle (inputs, pacings)

        pacings = bundle (pacing0, pacing1, pacing2, pacing3)

        (_, hasInput0) = unbundle inputs

        pacing0 = hasInput0
        pacing1 = hasInput0
        pacing2 = hasInput0
        pacing3 = hasInput0





---------------------------------------------------------------

type Tag = Unsigned 8
-- maxTag must be at least the size of the maximum window to avoid duplicate tags in the window
-- also to avoid having to do modulo operations maxTag must be at least as big as the largest offset
maxTag = 4 :: Tag
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
     

llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom ((Bool, Outputs), ((Bool, Bool, Bool, Bool)))
llc event = bundle (bundle (toPop, outputs), debugSignals)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        toPop = pure True

        (inputs, pacings) = unbundle poppedEvent
        input0 = inputs
        (_, input0HasData) = unbundle input0
        (p0, p1, p2, p3) = unbundle pacings

        tagIn0 = genTag input0HasData
        tagOut0 = genTag p0
        tagOut1 = genTag p1
        tagOut2 = genTag p2
        tagOut3 = genTag p3

        -- tag generation takes 1 cycle so we need to delay the input data
        (input0Data, _) = unbundle (delay (0, False) input0)

        -- delayed tags to be used in different levels 
        tagsDefault = (invalidTag, invalidTag, invalidTag, invalidTag, invalidTag)
        curTags = bundle (tagIn0, tagOut0, tagOut1, tagOut2, tagOut3)
        curTagsLevel1 = delayFor d1 tagsDefault curTags
        curTagsLevel2 = delayFor d2 tagsDefault curTags
        curTagsLevel3 = delayFor d3 tagsDefault curTags
        curTagsLevel4 = delayFor d4 tagsDefault curTags

        enIn0 = delayFor d1 False input0HasData
        enOut0 = delayFor d2 False p0
        enOut1 = delayFor d2 False p1
        enOut2 = delayFor d3 False p2
        enOut3 = delayFor d4 False p3

        output0Aktv = delayFor d5 False p0
        output1Aktv = delayFor d5 False p1
        output2Aktv = delayFor d5 False p2
        output3Aktv = delayFor d5 False p3

        -- Evaluation of input windows: level 0
        input0Win = input0Window enIn0 tagIn0 input0Data

        -- Evaluation of output 0: level 1
        (out0Level1TagIn0, out0Level1TagOut0, _, _, _) = unbundle curTagsLevel1
        out0 = outputStream0 enOut0 out0Level1TagOut0 out0Data0 
        out0Data0 = getMatchingTagFromNonVec <$> input0Win <*> out0Level1TagIn0 <*> (pure 0)

        -- Evaluation of output 1: level 1
        (out1Level1TagIn0, _, out1Level1TagOut1, _, _) = unbundle curTagsLevel1
        out1 = outputStream1 enOut1 out1Level1TagOut1 out1Data0 
        out1Data0 = getMatchingTagFromNonVec <$> input0Win <*> out1Level1TagIn0 <*> (pure 0)

        -- Evaluation of output 2: level 2
        (_, out2Level2TagOut0, _, out2Level2TagOut2, _) = unbundle curTagsLevel2
        out2 = outputStream2 enOut2 out2Level2TagOut2 out2Data0 
        out2Data0 = getMatchingTag <$> out0 <*> out2Level2TagOut0 <*> (pure 0)

        -- Evaluation of output 3: level 3
        (_, _, out3Level3TagOut1, out3Level3TagOut2, out3Level3TagOut3) = unbundle curTagsLevel3
        out3 = outputStream3 enOut3 out3Level3TagOut3 out3Data0 out3Data1 
        out3Data0 = getMatchingTag <$> out1 <*> out3Level3TagOut1 <*> (pure 0)
        out3Data1 = getMatchingTag <$> out2 <*> out3Level3TagOut2 <*> (pure 0)

        -- Outputing all results: level 4
        (_, level4TagOut0, level4TagOut1, level4TagOut2, level4TagOut3) = unbundle curTagsLevel4
        output0 = bundle (output0Data, output0Aktv)
        (_, output0Data) = unbundle (getMatchingTag <$> out0 <*> level4TagOut0 <*> (pure 0))
        output1 = bundle (output1Data, output1Aktv)
        (_, output1Data) = unbundle (getMatchingTag <$> out1 <*> level4TagOut1 <*> (pure 0))
        output2 = bundle (output2Data, output2Aktv)
        (_, output2Data) = unbundle (getMatchingTag <$> out2 <*> level4TagOut2 <*> (pure 0))
        output3 = bundle (output3Data, output3Aktv)
        (_, output3Data) = unbundle out3

        outputs = bundle (output0, output1, output2, output3)

        debugSignals = pacings

        genTag :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag
        genTag en = t
            where 
                t = register 1 (mux en next_t t)
                next_t = mux (t .==. (pure maxTag)) (pure 1) (t + 1)





input0Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
input0Window en tag val = result
    where result = register (invalidTag, 0) (mux en (bundle (tag, val)) result)



outputStream0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Vec 3 (Tag, Int))
outputStream0 en tag in0WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + 1
        (_, in0) = unbundle in0WithTag


outputStream1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Vec 3 (Tag, Int))
outputStream1 en tag in0WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + 10
        (_, in0) = unbundle in0WithTag


outputStream2 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
outputStream2 en tag out0WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out0 + 1
        (_, out0) = unbundle out0WithTag


outputStream3 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream3 en tag out1WithTag out2WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out1 + out2
        (_, out1) = unbundle out1WithTag
        (_, out2) = unbundle out2WithTag







---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Outputs, (QPush, QPop, QPushValid, QPopValid, (Bool, Bool, Bool, Bool)))
monitor inputs = bundle (outputs, debugSignals)
    where 
        (newEvent, event) = unbundle (hlc inputs)

        (qPushValid, qPopValid, qPopData) = unbundle (queue (bundle (qPush, qPop, qInptData)))
        qPush = newEvent
        qPop = toPop
        qInptData = event

        (llcOutput, llcDebug) = unbundle (llc (bundle (qPopValid, qPopData)))
        (toPop, outputs) = unbundle llcOutput

        llcPacings = llcDebug
        debugSignals = bundle (qPush, qPop, qPushValid, qPopValid, llcPacings)


---------------------------------------------------------------

topEntity :: Clock TestDomain -> Reset TestDomain -> Enable TestDomain -> 
    Signal TestDomain Inputs -> Signal TestDomain (Outputs, (QPush, QPop, QPushValid, QPopValid, (Bool, Bool, Bool, Bool)))
topEntity clk rst en inputs = exposeClockResetEnable (monitor inputs) clk rst en
