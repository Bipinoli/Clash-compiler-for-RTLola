{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Spec where

import Clash.Prelude

---------------------------------------------------------------

-- input x : Int
-- input y : Int
-- 
-- output a := x.offset(by: -1).defaults(to: x + y) +  1 + d.offset(by: -1).defaults(to: x)
-- output b := x.offset(by: -2).defaults(to: y) + 1
-- output c := a * b + x + y
-- output d := a + c + d.offset(by: -1).defaults(to: 0)
-- 
-- output e @2kHz := e.offset(by: -1).defaults(to: 0) + 1
-- output f @1kHz := x.aggregate(over: 0.003s, using: sum) 
-- output g := e + f + d.hold(or: 2)
-- output h @1kHz := d.aggregate(over: 0.004s, using: sum)

---------------------------------------------------------------

-- Evaluation Order
-- y, x
-- b, a
-- c
-- d, e, sw(x,f)
-- f, sw(d,h)
-- g, h

-- Memory Window
-- window x = 3
-- window d = 1
-- window e = 1
-- window sw(x,f) = 1
-- window y = 1
-- window f = 1
-- window sw(d,h) = 1
-- window g = 1
-- window a = 2
-- window h = 1
-- window c = 2
-- window b = 2

-- Pipeline Visualization
-- y,x         |             |             | y,x         |             |             | y,x         |             |             | y,x        
-- -----------------------------------------------------------------------------------------------------------------------------------------
--             | b,a         |             |             | b,a         |             |             | b,a         |             |            
-- -----------------------------------------------------------------------------------------------------------------------------------------
--             |             | c           |             |             | c           |             |             | c           |            
-- -----------------------------------------------------------------------------------------------------------------------------------------
--             |             |             | d,e,sw(x,f) |             |             | d,e,sw(x,f) |             |             | d,e,sw(x,f)
-- -----------------------------------------------------------------------------------------------------------------------------------------
--             |             |             |             | f,sw(d,h)   |             |             | f,sw(d,h)   |             |            
-- -----------------------------------------------------------------------------------------------------------------------------------------
--             |             |             |             |             | g,h         |             |             | g,h         |            
-- -----------------------------------------------------------------------------------------------------------------------------------------

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

data ValidInt = ValidInt {
    value :: Int,
    valid :: Bool
} deriving (Generic, NFDataX)


data Inputs = Inputs {
    input0 :: ValidInt,
    input1 :: ValidInt
} deriving (Generic, NFDataX)

data Outputs = Outputs {
    output0 :: ValidInt,
    output1 :: ValidInt,
    output2 :: ValidInt,
    output3 :: ValidInt,
    output4 :: ValidInt,
    output5 :: ValidInt,
    output6 :: ValidInt,
    output7 :: ValidInt
} deriving (Generic, NFDataX)


class Pacing a where getPacing :: a -> Bool

data PacingIn0 = PacingIn0 Bool deriving (Generic, NFDataX)
data PacingIn1 = PacingIn1 Bool deriving (Generic, NFDataX)
data PacingOut0 = PacingOut0 PacingIn0 PacingIn1 deriving (Generic, NFDataX)
data PacingOut1 = PacingOut1 PacingIn0 PacingIn1 deriving (Generic, NFDataX)
data PacingOut2 = PacingOut2 PacingIn0 PacingIn1 deriving (Generic, NFDataX)
data PacingOut3 = PacingOut3 PacingIn0 PacingIn1 deriving (Generic, NFDataX)
data PacingOut4 = PacingOut4 Bool deriving (Generic, NFDataX)
data PacingOut5 = PacingOut5 Bool deriving (Generic, NFDataX)
data PacingOut6 = PacingOut6 Bool deriving (Generic, NFDataX)
data PacingOut7 = PacingOut7 Bool deriving (Generic, NFDataX)

instance Pacing PacingIn0 where getPacing (PacingIn0 x) = x
instance Pacing PacingIn1 where getPacing (PacingIn1 x) = x
instance Pacing PacingOut0 where getPacing (PacingOut0 x0 x1) = getPacing x0 && getPacing x1
instance Pacing PacingOut1 where getPacing (PacingOut1 x0 x1) = getPacing x0 && getPacing x1
instance Pacing PacingOut2 where getPacing (PacingOut2 x0 x1) = getPacing x0 && getPacing x1
instance Pacing PacingOut3 where getPacing (PacingOut3 x0 x1) = getPacing x0 && getPacing x1
instance Pacing PacingOut4 where getPacing (PacingOut4 x) = x
instance Pacing PacingOut5 where getPacing (PacingOut5 x) = x
instance Pacing PacingOut6 where getPacing (PacingOut6 x) = x
instance Pacing PacingOut7 where getPacing (PacingOut7 x) = x

data Pacings = Pacings {
    pacingIn0 :: PacingIn0,
    pacingIn1 :: PacingIn1,
    pacingOut0 :: PacingOut0,
    pacingOut1 :: PacingOut1,
    pacingOut2 :: PacingOut2,
    pacingOut3 :: PacingOut3,
    pacingOut4 :: PacingOut4,
    pacingOut5 :: PacingOut5,
    pacingOut6 :: PacingOut6,
    pacingOut7 :: PacingOut7
} deriving (Generic, NFDataX)

data DebugEnables = DebugEnables {
    enableIn0 :: Bool,
    enableIn1 :: Bool,
    enableOut0 :: Bool,
    enableOut1 :: Bool,
    enableOut2 :: Bool,
    enableOut3 :: Bool,
    enableOut4 :: Bool,
    enableOut5 :: Bool,
    enableOut6 :: Bool,
    enableOut7 :: Bool
} deriving (Generic, NFDataX)

data Slides = Slides {
    slide0 :: Bool,
    slide1 :: Bool
} deriving (Generic, NFDataX)

type Tag = Unsigned 8

data Tags = Tags {
    input0 :: Tag,
    input1 :: Tag,
    output0 :: Tag,
    output1 :: Tag,
    output2 :: Tag,
    output3 :: Tag,
    output4 :: Tag,
    output5 :: Tag,
    output6 :: Tag,
    output7 :: Tag,
    slide0 :: Tag,
    slide1 :: Tag
} deriving (Generic, NFDataX)

type Event = (Inputs, Slides, Pacings)

nullEvent :: Event
nullEvent = (nullInputs, nullSlides, nullPacings)
nullInputs = Inputs (ValidInt 0 False) (ValidInt 0 False) 
nullSlides = Slides False False 
nullPacings = Pacings nullPacingIn0 nullPacingIn1 nullPacingOut0 nullPacingOut1 nullPacingOut2 nullPacingOut3 nullPacingOut4 nullPacingOut5 nullPacingOut6 nullPacingOut7 
nullPacingIn0 = PacingIn0 False
nullPacingIn1 = PacingIn1 False
nullPacingOut0 = PacingOut0 nullPacingIn0 nullPacingIn1 
nullPacingOut1 = PacingOut1 nullPacingIn0 nullPacingIn1 
nullPacingOut2 = PacingOut2 nullPacingIn0 nullPacingIn1 
nullPacingOut3 = PacingOut3 nullPacingIn0 nullPacingIn1 
nullPacingOut4 = PacingOut4 False 
nullPacingOut5 = PacingOut5 False 
nullPacingOut6 = PacingOut6 False 
nullPacingOut7 = PacingOut7 False 


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
        newEvent = hasInput0 .||. hasInput1 .||. timer0Over .||. timer1Over

        event = bundle (inputs, slides, pacings)

        slides = Slides <$> s0 <*> s1
        pacings = Pacings <$> pIn0 <*> pIn1 <*> pOut0 <*> pOut1 <*> pOut2 <*> pOut3 <*> pOut4 <*> pOut5 <*> pOut6 <*> pOut7

        hasInput0 = ((.valid). (.input0)) <$> inputs
        hasInput1 = ((.valid). (.input1)) <$> inputs

        pIn0 = PacingIn0 <$> hasInput0
        pIn1 = PacingIn1 <$> hasInput1
        pOut0 = PacingOut0 <$> pIn0 <*> pIn1
        pOut1 = PacingOut1 <$> pIn0 <*> pIn1
        pOut2 = PacingOut2 <$> pIn0 <*> pIn1
        pOut3 = PacingOut3 <$> pIn0 <*> pIn1
        pOut4 = PacingOut4 <$> timer0Over
        pOut5 = PacingOut5 <$> timer1Over
        pOut6 = PacingOut6 <$> timer1Over
        pOut7 = PacingOut7 <$> timer1Over

        s0 = timer1Over
        s1 = timer1Over

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

-- maxTag must be at least the size of the maximum window to avoid duplicate tags in the window
-- also to avoid having to do modulo operations maxTag must be at least as big as the largest offset
maxTag = 6 :: Tag
invalidTag = maxTag + 1

getOffset :: KnownNat n => Vec n (Tag, a) -> Tag -> Tag -> a -> a
getOffset win tag offset dflt = out
    where 
        offsetTag = earlierTag tag offset
        out = case findIndex (\(t, _) -> t == offsetTag) win of
            Just i -> let (_, v) = win !! i in v
            Nothing -> dflt

getMatchingTag :: KnownNat n => Vec n (Tag, a) -> Tag -> a -> a
getMatchingTag win tag dflt = out
    where 
        out = case findIndex (\(t, _) -> t == tag) win of
            Just i -> let (_, v) = win !! i in v
            Nothing -> dflt

getOffsetFromNonVec :: (Tag, a) -> Tag -> Tag -> a -> a
getOffsetFromNonVec (winTag, winData) tag offset dflt = out
    where 
        offsetTag = earlierTag tag offset
        out = if offsetTag == winTag then winData else dflt

getMatchingTagFromNonVec :: (Tag, a) -> Tag -> a -> a
getMatchingTagFromNonVec (tag, dta) tagToMatch dflt = if tag == tagToMatch then dta else dflt

getLatestValue :: KnownNat n => Vec (n + 1) (Tag, a) -> a -> a
getLatestValue win dflt =
  let (tag, dta) = last win
  in if tag == invalidTag then dflt else dta

getLatestValueFromNonVec :: (Tag, a) -> a -> a
getLatestValueFromNonVec (tag, dta) dflt = if tag == invalidTag then dflt else dta

earlierTag :: Tag -> Tag -> Tag
earlierTag curTag cyclesBefore = if curTag > cyclesBefore then curTag - cyclesBefore else curTag - cyclesBefore + maxTag

delayFor :: forall dom n a . (HiddenClockResetEnable dom, KnownNat n, NFDataX a) => SNat n -> a -> Signal dom a -> Signal dom a
delayFor n initVal sig = last delayedVec
    where
      delayedVec :: Vec (n + 1) (Signal dom a)
      delayedVec = iterateI (delay initVal) sig
     

llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom ((Bool, Outputs), (Slides, DebugEnables))
llc event = bundle (bundle (toPop, outputs), debugSignals)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        isPipelineReady = pipelineReady startNewPipeline
        startNewPipeline = mux (isPipelineReady .&&. isValidEvent) (pure True) (pure False)
        toPop = isPipelineReady .&&. not <$> startNewPipeline

        (inputs, slides, pacings) = unbundle poppedEvent

        input0 = (.input0) <$> inputs
        input1 = (.input1) <$> inputs

        slide0 = (.slide0) <$> slides
        slide1 = (.slide1) <$> slides

        pIn0 = (.pacingIn0) <$> pacings
        pIn1 = (.pacingIn1) <$> pacings
        pOut0 = (.pacingOut0) <$> pacings
        pOut1 = (.pacingOut1) <$> pacings
        pOut2 = (.pacingOut2) <$> pacings
        pOut3 = (.pacingOut3) <$> pacings
        pOut4 = (.pacingOut4) <$> pacings
        pOut5 = (.pacingOut5) <$> pacings
        pOut6 = (.pacingOut6) <$> pacings
        pOut7 = (.pacingOut7) <$> pacings
        
        tIn1 = genTag (getPacing <$> pIn1)
        tIn0 = genTag (getPacing <$> pIn0)
        tOut1 = genTag (getPacing <$> pOut1)
        tOut0 = genTag (getPacing <$> pOut0)
        tOut2 = genTag (getPacing <$> pOut2)
        tOut3 = genTag (getPacing <$> pOut3)
        tOut4 = genTag (getPacing <$> pOut4)
        tSw0 = genTag (getPacing <$> pIn0)
        tOut5 = genTag (getPacing <$> pOut5)
        tSw1 = genTag (getPacing <$> pOut3)
        tOut6 = genTag (getPacing <$> pOut6)
        tOut7 = genTag (getPacing <$> pOut7)

        -- tag generation takes 1 cycle so we need to delay the input data
        input0Data = delay 0 (((.value). (.input0)) <$> inputs)
        input1Data = delay 0 (((.value). (.input1)) <$> inputs)

        -- delayed tags to be used in different levels 
        tagsDefault = Tags nullT nullT nullT nullT nullT nullT nullT nullT nullT nullT nullT nullT 
        curTags = Tags <$> tIn0 <*> tIn1 <*> tOut0 <*> tOut1 <*> tOut2 <*> tOut3 <*> tOut4 <*> tOut5 <*> tOut6 <*> tOut7 <*> tSw0 <*> tSw1
        curTagsLevel1 = delayFor d1 tagsDefault curTags
        curTagsLevel2 = delayFor d2 tagsDefault curTags
        curTagsLevel3 = delayFor d3 tagsDefault curTags
        curTagsLevel4 = delayFor d4 tagsDefault curTags
        curTagsLevel5 = delayFor d5 tagsDefault curTags
        curTagsLevel6 = delayFor d6 tagsDefault curTags
        nullT = invalidTag

        enIn1 = delayFor d1 nullPacingIn1 pIn1
        enIn0 = delayFor d1 nullPacingIn0 pIn0
        enOut1 = delayFor d2 nullPacingOut1 pOut1
        enOut0 = delayFor d2 nullPacingOut0 pOut0
        enOut2 = delayFor d3 nullPacingOut2 pOut2
        enOut3 = delayFor d4 nullPacingOut3 pOut3
        enOut4 = delayFor d4 nullPacingOut4 pOut4
        enSw0 = delayFor d4 nullPacingIn0 pIn0
        sld0 = delayFor d4 False slide0
        enOut5 = delayFor d5 nullPacingOut5 pOut5
        enSw1 = delayFor d5 nullPacingOut3 pOut3
        sld1 = delayFor d5 False slide1
        enOut6 = delayFor d6 nullPacingOut6 pOut6
        enOut7 = delayFor d6 nullPacingOut7 pOut7

        output0Aktv = delayFor d7 False (getPacing <$> pOut0)
        output1Aktv = delayFor d7 False (getPacing <$> pOut1)
        output2Aktv = delayFor d7 False (getPacing <$> pOut2)
        output3Aktv = delayFor d7 False (getPacing <$> pOut3)
        output4Aktv = delayFor d7 False (getPacing <$> pOut4)
        output5Aktv = delayFor d7 False (getPacing <$> pOut5)
        output6Aktv = delayFor d7 False (getPacing <$> pOut6)
        output7Aktv = delayFor d7 False (getPacing <$> pOut7)

        -- Evaluation of input windows: level 0
        input0Win = input0Window enIn0 tIn0 input0Data
        input1Win = input1Window enIn1 tIn1 input1Data

        -- Evaluation of output 0: level 1
        out0 = outputStream0 enOut0 ((.output0) <$> curTagsLevel1) out0Data0 out0Data1 
        out0Data0 = getOffset <$> input0Win <*> ((.input0) <$> curTagsLevel1) <*> (pure 1) <*> out0Data0Dflt
        out0Data0Dflt = out0Data0DfltData0 + out0Data0DfltData1
        out0Data0DfltData0 =  getMatchingTag <$> input0Win <*> ((.input0) <$> curTagsLevel1) <*> (pure (0))
        (_, out0Data0DfltData1) = unbundle input1Win
        out0Data1 = getOffsetFromNonVec <$> out3 <*> ((.output3) <$> curTagsLevel1) <*> (pure 1) <*> out0Data1Dflt
        out0Data1Dflt =  getMatchingTag <$> input0Win <*> ((.input0) <$> curTagsLevel1) <*> (pure (0))

        -- Evaluation of output 1: level 1
        out1 = outputStream1 enOut1 ((.output1) <$> curTagsLevel1) out1Data0 
        out1Data0 = getOffset <$> input0Win <*> ((.input0) <$> curTagsLevel1) <*> (pure 2) <*> out1Data0Dflt
        (_, out1Data0Dflt) = unbundle input1Win

        -- Evaluation of output 2: level 2
        out2 = outputStream2 enOut2 ((.output2) <$> curTagsLevel2) out2Data0 out2Data1 out2Data2 out2Data3 
        out2Data0 = getMatchingTag <$> out0 <*> ((.output0) <$> curTagsLevel2) <*> (pure (0))
        out2Data1 = getMatchingTag <$> out1 <*> ((.output1) <$> curTagsLevel2) <*> (pure (0))
        out2Data2 = getMatchingTag <$> input0Win <*> ((.input0) <$> curTagsLevel2) <*> (pure (0))
        out2Data3 = getMatchingTagFromNonVec <$> input1Win <*> ((.input1) <$> curTagsLevel2) <*> (pure (0))

        -- Evaluation of output 3: level 3
        out3 = outputStream3 enOut3 ((.output3) <$> curTagsLevel3) out3Data0 out3Data1 out3Data2 
        out3Data0 = getMatchingTag <$> out0 <*> ((.output0) <$> curTagsLevel3) <*> (pure (0))
        out3Data1 = getMatchingTag <$> out2 <*> ((.output2) <$> curTagsLevel3) <*> (pure (0))
        out3Data2 = getOffsetFromNonVec <$> out3 <*> ((.output3) <$> curTagsLevel3) <*> (pure 1) <*> out3Data2Dflt
        out3Data2Dflt = pure (0)

        -- Evaluation of output 4: level 3
        out4 = outputStream4 enOut4 ((.output4) <$> curTagsLevel3) out4Data0 
        out4Data0 = getOffsetFromNonVec <$> out4 <*> ((.output4) <$> curTagsLevel3) <*> (pure 1) <*> out4Data0Dflt
        out4Data0Dflt = pure (0)

        -- Evaluation of output 5: level 4
        out5 = outputStream5 enOut5 ((.output5) <$> curTagsLevel4) out5Data0 
        (_, out5Data0) = unbundle sw0

        -- Evaluation of output 6: level 5
        out6 = outputStream6 enOut6 ((.output6) <$> curTagsLevel5) out6Data0 out6Data1 out6Data2 
        out6Data0 = getMatchingTagFromNonVec <$> out4 <*> ((.output4) <$> curTagsLevel5) <*> (pure (0))
        out6Data1 = getMatchingTagFromNonVec <$> out5 <*> ((.output5) <$> curTagsLevel5) <*> (pure (0))
        out6Data2 = getLatestValueFromNonVec <$> out3 <*> out6Data2Dflt
        out6Data2Dflt = pure (2)

        -- Evaluation of output 7: level 5
        out7 = outputStream7 enOut7 ((.output7) <$> curTagsLevel5) out7Data0 
        (_, out7Data0) = unbundle sw1

        -- Evaluation of sliding window 0: level 3
        sw0 = slidingWindow0 enSw0 sld0 ((.slide0) <$> curTagsLevel3) sw0Data
        sw0Data = getMatchingTag <$> input0Win <*> ((.input0) <$> curTagsLevel3) <*> (pure 0)

        -- Evaluation of sliding window 1: level 4
        sw1 = slidingWindow1 enSw1 sld1 ((.slide1) <$> curTagsLevel4) sw1Data
        (_, sw1Data) = unbundle out3

        -- Outputing all results: level 6
        output0 = ValidInt <$> output0Data <*> output0Aktv
        output0Data = getMatchingTag <$> out0 <*> ((.output0) <$> curTagsLevel6) <*> (pure 0)
        output1 = ValidInt <$> output1Data <*> output1Aktv
        output1Data = getMatchingTag <$> out1 <*> ((.output1) <$> curTagsLevel6) <*> (pure 0)
        output2 = ValidInt <$> output2Data <*> output2Aktv
        output2Data = getMatchingTag <$> out2 <*> ((.output2) <$> curTagsLevel6) <*> (pure 0)
        output3 = ValidInt <$> output3Data <*> output3Aktv
        (_, output3Data) = unbundle out3
        output4 = ValidInt <$> output4Data <*> output4Aktv
        (_, output4Data) = unbundle out4
        output5 = ValidInt <$> output5Data <*> output5Aktv
        (_, output5Data) = unbundle out5
        output6 = ValidInt <$> output6Data <*> output6Aktv
        (_, output6Data) = unbundle out6
        output7 = ValidInt <$> output7Data <*> output7Aktv
        (_, output7Data) = unbundle out7

        outputs = Outputs <$> output0 <*> output1 <*> output2 <*> output3 <*> output4 <*> output5 <*> output6 <*> output7

        debugSignals = bundle (slides, debugEnables)
        debugEnables = DebugEnables <$>
                            (getPacing <$> enIn0) <*>
                            (getPacing <$> enIn1) <*>
                            (getPacing <$> enOut0) <*>
                            (getPacing <$> enOut1) <*>
                            (getPacing <$> enOut2) <*>
                            (getPacing <$> enOut3) <*>
                            (getPacing <$> enOut4) <*>
                            (getPacing <$> enOut5) <*>
                            (getPacing <$> enOut6) <*>
                            (getPacing <$> enOut7)

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



input0Window :: HiddenClockResetEnable dom => Signal dom PacingIn0 -> Signal dom Tag -> Signal dom Int -> Signal dom (Vec 3 (Tag, Int))
input0Window en tag val = result
    where result = register (repeat (invalidTag, 0)) (mux (getPacing <$> en) ((<<+) <$> result <*> (bundle (tag, val))) result)


input1Window :: HiddenClockResetEnable dom => Signal dom PacingIn1 -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
input1Window en tag val = result
    where result = register (invalidTag, 0) (mux (getPacing <$> en) (bundle (tag, val)) result)



outputStream0 :: HiddenClockResetEnable dom => Signal dom PacingOut0 -> Signal dom Tag -> Signal dom Int -> Signal dom Int -> Signal dom (Vec 2 (Tag, Int))
outputStream0 en tag in0_00 out3_1 = result
    where
        result = register (repeat (invalidTag, 0)) (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0_00 + 1 + out3_1


outputStream1 :: HiddenClockResetEnable dom => Signal dom PacingOut1 -> Signal dom Tag -> Signal dom Int -> Signal dom (Vec 2 (Tag, Int))
outputStream1 en tag in0_0 = result
    where
        result = register (repeat (invalidTag, 0)) (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0_0 + 1


outputStream2 :: HiddenClockResetEnable dom => Signal dom PacingOut2 -> Signal dom Tag -> Signal dom Int -> Signal dom Int -> Signal dom Int -> Signal dom Int -> Signal dom (Vec 2 (Tag, Int))
outputStream2 en tag out0_000 out1_001 in0_01 in1_1 = result
    where
        result = register (repeat (invalidTag, 0)) (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out0_000 * out1_001 + in0_01 + in1_1


outputStream3 :: HiddenClockResetEnable dom => Signal dom PacingOut3 -> Signal dom Tag -> Signal dom Int -> Signal dom Int -> Signal dom Int -> Signal dom (Tag, Int)
outputStream3 en tag out0_00 out2_01 out3_1 = result
    where
        result = register (invalidTag, 0) (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out0_00 + out2_01 + out3_1


outputStream4 :: HiddenClockResetEnable dom => Signal dom PacingOut4 -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
outputStream4 en tag out4_0 = result
    where
        result = register (invalidTag, 0) (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out4_0 + 1


outputStream5 :: HiddenClockResetEnable dom => Signal dom PacingOut5 -> Signal dom Tag -> Signal dom (Vec 4 Int) -> Signal dom (Tag, Int)
outputStream5 en tag sw0 = result
    where
        result = register (invalidTag, 0) (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = (merge0 <$> sw0)
        merge0 :: Vec 4 Int -> Int
        merge0 win = fold windowBucketFunc0 (tail win)


outputStream6 :: HiddenClockResetEnable dom => Signal dom PacingOut6 -> Signal dom Tag -> Signal dom Int -> Signal dom Int -> Signal dom Int -> Signal dom (Tag, Int)
outputStream6 en tag out4_00 out5_01 out3_1 = result
    where
        result = register (invalidTag, 0) (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out4_00 + out5_01 + out3_1


outputStream7 :: HiddenClockResetEnable dom => Signal dom PacingOut7 -> Signal dom Tag -> Signal dom (Vec 5 Int) -> Signal dom (Tag, Int)
outputStream7 en tag sw1 = result
    where
        result = register (invalidTag, 0) (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = (merge1 <$> sw1)
        merge1 :: Vec 5 Int -> Int
        merge1 win = fold windowBucketFunc1 (tail win)



windowBucketFunc0 :: Int -> Int -> Int
windowBucketFunc0 acc item = acc + item

windowBucketFunc1 :: Int -> Int -> Int
windowBucketFunc1 acc item = acc + item


slidingWindow0 :: HiddenClockResetEnable dom => Signal dom PacingIn0 -> Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, (Vec 4 Int)) 
slidingWindow0 newData slide tag inpt = window
    where
        window = register (invalidTag, dflt) (mux en next window)
        next = bundle (tag, nextWindow <$> (snd <$> window) <*> slide <*> (getPacing <$> newData) <*> inpt)
        en = (getPacing <$> newData) .||. slide
        dflt = repeat 0 :: Vec 4 Int

        nextWindow :: Vec 4 Int -> Bool -> Bool -> Int -> Vec 4 Int
        nextWindow win toSlide newData dta = out
            where
                out = case (toSlide, newData) of
                    (False, False) -> win
                    (False, True) -> lastBucketUpdated
                    (True, False) -> 0 +>> win
                    (True, True) -> 0 +>> lastBucketUpdated
                lastBucketUpdated = replace 0 (windowBucketFunc0 (head win) dta) win

slidingWindow1 :: HiddenClockResetEnable dom => Signal dom PacingOut3 -> Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, (Vec 5 Int)) 
slidingWindow1 newData slide tag inpt = window
    where
        window = register (invalidTag, dflt) (mux en next window)
        next = bundle (tag, nextWindow <$> (snd <$> window) <*> slide <*> (getPacing <$> newData) <*> inpt)
        en = (getPacing <$> newData) .||. slide
        dflt = repeat 0 :: Vec 5 Int

        nextWindow :: Vec 5 Int -> Bool -> Bool -> Int -> Vec 5 Int
        nextWindow win toSlide newData dta = out
            where
                out = case (toSlide, newData) of
                    (False, False) -> win
                    (False, True) -> lastBucketUpdated
                    (True, False) -> 0 +>> win
                    (True, True) -> 0 +>> lastBucketUpdated
                lastBucketUpdated = replace 0 (windowBucketFunc1 (head win) dta) win




---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom => Signal dom Inputs ->Signal dom (Outputs, (QPush, QPop, QPushValid, QPopValid, Slides, DebugEnables))
monitor inputs = bundle (outputs, debugSignals)
    where 
        (newEvent, event) = unbundle (hlc inputs)

        (qPushValid, qPopValid, qPopData) = unbundle (queue (bundle (qPush, qPop, qInptData)))
        qPush = newEvent
        qPop = toPop
        qInptData = event

        (llcOutput, llcDebug) = unbundle (llc (bundle (qPopValid, qPopData)))
        (toPop, outputs) = unbundle llcOutput

        (llcSlides, debugEnables) = unbundle llcDebug
        debugSignals = bundle (qPush, qPop, qPushValid, qPopValid, llcSlides, debugEnables)


---------------------------------------------------------------

topEntity :: Clock TestDomain -> Reset TestDomain -> Enable TestDomain -> 
    Signal TestDomain Inputs -> Signal TestDomain (Outputs, (QPush, QPop, QPushValid, QPopValid, Slides, DebugEnables))
topEntity clk rst en inputs = exposeClockResetEnable (monitor inputs) clk rst en
