{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Spec where

import Clash.Prelude

---------------------------------------------------------------

-- input x : Int
-- 
-- output a := x + c.offset(by: -1).defaults(to: 0)
-- output b := a + 1
-- output c := b + 1
-- output d := b + 1
-- output e := c + 1
-- output f := d + 1
-- output g := e + l.offset(by: -1).defaults(to: 0)
-- output h := f + 1
-- output i := g + k
-- output j := h + 1
-- output k := j + 1
-- output l := i + 1

---------------------------------------------------------------

-- Evaluation Order
-- x
-- a
-- b
-- c, d
-- f, e
-- g, h
-- j
-- k
-- i
-- l

-- Memory Window
-- window k = 1
-- window x = 1
-- window b = 2
-- window f = 2
-- window h = 1
-- window e = 2
-- window c = 2
-- window i = 1
-- window l = 1
-- window d = 2
-- window j = 1
-- window g = 1
-- window a = 2

-- Pipeline Visualization
-- x   |     |     |     |     | x   |     |     |     |    
-- ---------------------------------------------------------
--     | a   |     |     |     |     | a   |     |     |    
-- ---------------------------------------------------------
--     |     | b   |     |     |     |     | b   |     |    
-- ---------------------------------------------------------
--     |     |     | c,d |     |     |     |     | c,d |    
-- ---------------------------------------------------------
--     |     |     |     | f,e |     |     |     |     | f,e
-- ---------------------------------------------------------
--     |     |     |     |     | g,h |     |     |     |    
-- ---------------------------------------------------------
--     |     |     |     |     |     | j   |     |     |    
-- ---------------------------------------------------------
--     |     |     |     |     |     |     | k   |     |    
-- ---------------------------------------------------------
--     |     |     |     |     |     |     |     | i   |    
-- ---------------------------------------------------------
--     |     |     |     |     |     |     |     |     | l  
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
-- output10 = k
-- output11 = l

---------------------------------------------------------------

data ValidInt = ValidInt {
    value :: Int,
    valid :: Bool
} deriving (Generic, NFDataX)


-- using newtype to avoid flattening of data
-- https://clash-lang.discourse.group/t/how-to-avoid-flattening-of-fields-in-record/79/5
newtype Inputs = Inputs {
    input0 :: ValidInt
} deriving (Generic, NFDataX)

data Outputs = Outputs {
    output0 :: ValidInt,
    output1 :: ValidInt,
    output2 :: ValidInt,
    output3 :: ValidInt,
    output4 :: ValidInt,
    output5 :: ValidInt,
    output6 :: ValidInt,
    output7 :: ValidInt,
    output8 :: ValidInt,
    output9 :: ValidInt,
    output10 :: ValidInt,
    output11 :: ValidInt
} deriving (Generic, NFDataX)


class Pacing a where getPacing :: a -> Bool

data PacingIn0 = PacingIn0 Bool deriving (Generic, NFDataX)
data PacingOut0 = PacingOut0 PacingIn0 deriving (Generic, NFDataX)
data PacingOut1 = PacingOut1 PacingIn0 deriving (Generic, NFDataX)
data PacingOut2 = PacingOut2 PacingIn0 deriving (Generic, NFDataX)
data PacingOut3 = PacingOut3 PacingIn0 deriving (Generic, NFDataX)
data PacingOut4 = PacingOut4 PacingIn0 deriving (Generic, NFDataX)
data PacingOut5 = PacingOut5 PacingIn0 deriving (Generic, NFDataX)
data PacingOut6 = PacingOut6 PacingIn0 deriving (Generic, NFDataX)
data PacingOut7 = PacingOut7 PacingIn0 deriving (Generic, NFDataX)
data PacingOut8 = PacingOut8 PacingIn0 deriving (Generic, NFDataX)
data PacingOut9 = PacingOut9 PacingIn0 deriving (Generic, NFDataX)
data PacingOut10 = PacingOut10 PacingIn0 deriving (Generic, NFDataX)
data PacingOut11 = PacingOut11 PacingIn0 deriving (Generic, NFDataX)

instance Pacing PacingIn0 where getPacing (PacingIn0 x) = x
instance Pacing PacingOut0 where getPacing (PacingOut0 x) = getPacing x
instance Pacing PacingOut1 where getPacing (PacingOut1 x) = getPacing x
instance Pacing PacingOut2 where getPacing (PacingOut2 x) = getPacing x
instance Pacing PacingOut3 where getPacing (PacingOut3 x) = getPacing x
instance Pacing PacingOut4 where getPacing (PacingOut4 x) = getPacing x
instance Pacing PacingOut5 where getPacing (PacingOut5 x) = getPacing x
instance Pacing PacingOut6 where getPacing (PacingOut6 x) = getPacing x
instance Pacing PacingOut7 where getPacing (PacingOut7 x) = getPacing x
instance Pacing PacingOut8 where getPacing (PacingOut8 x) = getPacing x
instance Pacing PacingOut9 where getPacing (PacingOut9 x) = getPacing x
instance Pacing PacingOut10 where getPacing (PacingOut10 x) = getPacing x
instance Pacing PacingOut11 where getPacing (PacingOut11 x) = getPacing x

data Pacings = Pacings {
    pacingIn0 :: PacingIn0,
    pacingOut0 :: PacingOut0,
    pacingOut1 :: PacingOut1,
    pacingOut2 :: PacingOut2,
    pacingOut3 :: PacingOut3,
    pacingOut4 :: PacingOut4,
    pacingOut5 :: PacingOut5,
    pacingOut6 :: PacingOut6,
    pacingOut7 :: PacingOut7,
    pacingOut8 :: PacingOut8,
    pacingOut9 :: PacingOut9,
    pacingOut10 :: PacingOut10,
    pacingOut11 :: PacingOut11
} deriving (Generic, NFDataX)

data DebugEnables = DebugEnables {
    enableIn0 :: Bool,
    enableOut0 :: Bool,
    enableOut1 :: Bool,
    enableOut2 :: Bool,
    enableOut3 :: Bool,
    enableOut4 :: Bool,
    enableOut5 :: Bool,
    enableOut6 :: Bool,
    enableOut7 :: Bool,
    enableOut8 :: Bool,
    enableOut9 :: Bool,
    enableOut10 :: Bool,
    enableOut11 :: Bool
} deriving (Generic, NFDataX)


type Tag = Unsigned 8

data Tags = Tags {
    input0 :: Tag,
    output0 :: Tag,
    output1 :: Tag,
    output2 :: Tag,
    output3 :: Tag,
    output4 :: Tag,
    output5 :: Tag,
    output6 :: Tag,
    output7 :: Tag,
    output8 :: Tag,
    output9 :: Tag,
    output10 :: Tag,
    output11 :: Tag
} deriving (Generic, NFDataX)

type Event = (Inputs, Pacings)

nullEvent :: Event
nullEvent = (nullInputs, nullPacings)
nullInputs = Inputs (ValidInt 0 False) 
nullPacings = Pacings nullPacingIn0 nullPacingOut0 nullPacingOut1 nullPacingOut2 nullPacingOut3 nullPacingOut4 nullPacingOut5 nullPacingOut6 nullPacingOut7 nullPacingOut8 nullPacingOut9 nullPacingOut10 nullPacingOut11 
nullPacingIn0 = PacingIn0 False
nullPacingOut0 = PacingOut0 nullPacingIn0 
nullPacingOut1 = PacingOut1 nullPacingIn0 
nullPacingOut2 = PacingOut2 nullPacingIn0 
nullPacingOut3 = PacingOut3 nullPacingIn0 
nullPacingOut4 = PacingOut4 nullPacingIn0 
nullPacingOut5 = PacingOut5 nullPacingIn0 
nullPacingOut6 = PacingOut6 nullPacingIn0 
nullPacingOut7 = PacingOut7 nullPacingIn0 
nullPacingOut8 = PacingOut8 nullPacingIn0 
nullPacingOut9 = PacingOut9 nullPacingIn0 
nullPacingOut10 = PacingOut10 nullPacingIn0 
nullPacingOut11 = PacingOut11 nullPacingIn0 


---------------------------------------------------------------

type QMemSize = 6

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

        pacings = Pacings <$> pIn0 <*> pOut0 <*> pOut1 <*> pOut2 <*> pOut3 <*> pOut4 <*> pOut5 <*> pOut6 <*> pOut7 <*> pOut8 <*> pOut9 <*> pOut10 <*> pOut11

        hasInput0 = ((.valid). (.input0)) <$> inputs

        pIn0 = PacingIn0 <$> hasInput0
        pOut0 = PacingOut0 <$> pIn0
        pOut1 = PacingOut1 <$> pIn0
        pOut2 = PacingOut2 <$> pIn0
        pOut3 = PacingOut3 <$> pIn0
        pOut4 = PacingOut4 <$> pIn0
        pOut5 = PacingOut5 <$> pIn0
        pOut6 = PacingOut6 <$> pIn0
        pOut7 = PacingOut7 <$> pIn0
        pOut8 = PacingOut8 <$> pIn0
        pOut9 = PacingOut9 <$> pIn0
        pOut10 = PacingOut10 <$> pIn0
        pOut11 = PacingOut11 <$> pIn0





---------------------------------------------------------------

-- maxTag must be at least the size of the maximum window to avoid duplicate tags in the window
-- also to avoid having to do modulo operations maxTag must be at least as big as the largest offset
maxTag = 3 :: Tag
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
     

llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom ((Bool, Outputs), (DebugEnables))
llc event = bundle (bundle (toPop, outputs), debugSignals)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        isPipelineReady = pipelineReady startNewPipeline
        startNewPipeline = mux (isPipelineReady .&&. isValidEvent) (pure True) (pure False)
        toPop = isPipelineReady .&&. not <$> startNewPipeline

        (inputs, pacings) = unbundle poppedEvent

        input0 = (.input0) <$> inputs


        pIn0 = (.pacingIn0) <$> pacings
        pOut0 = (.pacingOut0) <$> pacings
        pOut1 = (.pacingOut1) <$> pacings
        pOut2 = (.pacingOut2) <$> pacings
        pOut3 = (.pacingOut3) <$> pacings
        pOut4 = (.pacingOut4) <$> pacings
        pOut5 = (.pacingOut5) <$> pacings
        pOut6 = (.pacingOut6) <$> pacings
        pOut7 = (.pacingOut7) <$> pacings
        pOut8 = (.pacingOut8) <$> pacings
        pOut9 = (.pacingOut9) <$> pacings
        pOut10 = (.pacingOut10) <$> pacings
        pOut11 = (.pacingOut11) <$> pacings
        
        tIn0 = genTag (getPacing <$> pIn0)
        tOut0 = genTag (getPacing <$> pOut0)
        tOut1 = genTag (getPacing <$> pOut1)
        tOut2 = genTag (getPacing <$> pOut2)
        tOut3 = genTag (getPacing <$> pOut3)
        tOut5 = genTag (getPacing <$> pOut5)
        tOut4 = genTag (getPacing <$> pOut4)
        tOut6 = genTag (getPacing <$> pOut6)
        tOut7 = genTag (getPacing <$> pOut7)
        tOut9 = genTag (getPacing <$> pOut9)
        tOut10 = genTag (getPacing <$> pOut10)
        tOut8 = genTag (getPacing <$> pOut8)
        tOut11 = genTag (getPacing <$> pOut11)

        -- tag generation takes 1 cycle so we need to delay the input data
        input0Data = delay 0 (((.value). (.input0)) <$> inputs)

        -- delayed tags to be used in different levels 
        tagsDefault = Tags nullT nullT nullT nullT nullT nullT nullT nullT nullT nullT nullT nullT nullT 
        curTags = Tags <$> tIn0 <*> tOut0 <*> tOut1 <*> tOut2 <*> tOut3 <*> tOut4 <*> tOut5 <*> tOut6 <*> tOut7 <*> tOut8 <*> tOut9 <*> tOut10 <*> tOut11
        curTagsLevel1 = delayFor d1 tagsDefault curTags
        curTagsLevel2 = delayFor d2 tagsDefault curTags
        curTagsLevel3 = delayFor d3 tagsDefault curTags
        curTagsLevel4 = delayFor d4 tagsDefault curTags
        curTagsLevel5 = delayFor d5 tagsDefault curTags
        curTagsLevel6 = delayFor d6 tagsDefault curTags
        curTagsLevel7 = delayFor d7 tagsDefault curTags
        curTagsLevel8 = delayFor d8 tagsDefault curTags
        curTagsLevel9 = delayFor d9 tagsDefault curTags
        curTagsLevel10 = delayFor d10 tagsDefault curTags
        nullT = invalidTag

        enIn0 = delayFor d1 nullPacingIn0 pIn0
        enOut0 = delayFor d2 nullPacingOut0 pOut0
        enOut1 = delayFor d3 nullPacingOut1 pOut1
        enOut2 = delayFor d4 nullPacingOut2 pOut2
        enOut3 = delayFor d4 nullPacingOut3 pOut3
        enOut5 = delayFor d5 nullPacingOut5 pOut5
        enOut4 = delayFor d5 nullPacingOut4 pOut4
        enOut6 = delayFor d6 nullPacingOut6 pOut6
        enOut7 = delayFor d6 nullPacingOut7 pOut7
        enOut9 = delayFor d7 nullPacingOut9 pOut9
        enOut10 = delayFor d8 nullPacingOut10 pOut10
        enOut8 = delayFor d9 nullPacingOut8 pOut8
        enOut11 = delayFor d10 nullPacingOut11 pOut11

        output0Aktv = delayFor d11 False (getPacing <$> pOut0)
        output1Aktv = delayFor d11 False (getPacing <$> pOut1)
        output2Aktv = delayFor d11 False (getPacing <$> pOut2)
        output3Aktv = delayFor d11 False (getPacing <$> pOut3)
        output4Aktv = delayFor d11 False (getPacing <$> pOut4)
        output5Aktv = delayFor d11 False (getPacing <$> pOut5)
        output6Aktv = delayFor d11 False (getPacing <$> pOut6)
        output7Aktv = delayFor d11 False (getPacing <$> pOut7)
        output8Aktv = delayFor d11 False (getPacing <$> pOut8)
        output9Aktv = delayFor d11 False (getPacing <$> pOut9)
        output10Aktv = delayFor d11 False (getPacing <$> pOut10)
        output11Aktv = delayFor d11 False (getPacing <$> pOut11)

        -- Evaluation of input windows: level 0
        input0Win = input0Window enIn0 tIn0 input0Data

        -- Evaluation of output 0: level 1
        out0 = outputStream0 enOut0 ((.output0) <$> curTagsLevel1) out0Data0 out0Data1 
        out0Data0 = getMatchingTagFromNonVec <$> input0Win <*> ((.input0) <$> curTagsLevel1) <*> (pure (0))
        out0Data1 = getOffset <$> out2 <*> ((.output2) <$> curTagsLevel1) <*> (pure 1) <*> out0Data1Dflt
        out0Data1Dflt = pure (0)

        -- Evaluation of output 1: level 2
        out1 = outputStream1 enOut1 ((.output1) <$> curTagsLevel2) out1Data0 
        out1Data0 = getMatchingTag <$> out0 <*> ((.output0) <$> curTagsLevel2) <*> (pure (0))

        -- Evaluation of output 2: level 3
        out2 = outputStream2 enOut2 ((.output2) <$> curTagsLevel3) out2Data0 
        out2Data0 = getMatchingTag <$> out1 <*> ((.output1) <$> curTagsLevel3) <*> (pure (0))

        -- Evaluation of output 3: level 3
        out3 = outputStream3 enOut3 ((.output3) <$> curTagsLevel3) out3Data0 
        out3Data0 = getMatchingTag <$> out1 <*> ((.output1) <$> curTagsLevel3) <*> (pure (0))

        -- Evaluation of output 4: level 4
        out4 = outputStream4 enOut4 ((.output4) <$> curTagsLevel4) out4Data0 
        out4Data0 = getMatchingTag <$> out2 <*> ((.output2) <$> curTagsLevel4) <*> (pure (0))

        -- Evaluation of output 5: level 4
        out5 = outputStream5 enOut5 ((.output5) <$> curTagsLevel4) out5Data0 
        out5Data0 = getMatchingTag <$> out3 <*> ((.output3) <$> curTagsLevel4) <*> (pure (0))

        -- Evaluation of output 6: level 5
        out6 = outputStream6 enOut6 ((.output6) <$> curTagsLevel5) out6Data0 out6Data1 
        out6Data0 = getMatchingTag <$> out4 <*> ((.output4) <$> curTagsLevel5) <*> (pure (0))
        out6Data1 = getOffsetFromNonVec <$> out11 <*> ((.output11) <$> curTagsLevel5) <*> (pure 1) <*> out6Data1Dflt
        out6Data1Dflt = pure (0)

        -- Evaluation of output 7: level 5
        out7 = outputStream7 enOut7 ((.output7) <$> curTagsLevel5) out7Data0 
        out7Data0 = getMatchingTag <$> out5 <*> ((.output5) <$> curTagsLevel5) <*> (pure (0))

        -- Evaluation of output 8: level 8
        out8 = outputStream8 enOut8 ((.output8) <$> curTagsLevel8) out8Data0 out8Data1 
        out8Data0 = getMatchingTagFromNonVec <$> out6 <*> ((.output6) <$> curTagsLevel8) <*> (pure (0))
        out8Data1 = getMatchingTagFromNonVec <$> out10 <*> ((.output10) <$> curTagsLevel8) <*> (pure (0))

        -- Evaluation of output 9: level 6
        out9 = outputStream9 enOut9 ((.output9) <$> curTagsLevel6) out9Data0 
        out9Data0 = getMatchingTagFromNonVec <$> out7 <*> ((.output7) <$> curTagsLevel6) <*> (pure (0))

        -- Evaluation of output 10: level 7
        out10 = outputStream10 enOut10 ((.output10) <$> curTagsLevel7) out10Data0 
        out10Data0 = getMatchingTagFromNonVec <$> out9 <*> ((.output9) <$> curTagsLevel7) <*> (pure (0))

        -- Evaluation of output 11: level 9
        out11 = outputStream11 enOut11 ((.output11) <$> curTagsLevel9) out11Data0 
        out11Data0 = getMatchingTagFromNonVec <$> out8 <*> ((.output8) <$> curTagsLevel9) <*> (pure (0))

        -- Outputing all results: level 10
        output0 = ValidInt <$> output0Data <*> output0Aktv
        output0Data = getMatchingTag <$> out0 <*> ((.output0) <$> curTagsLevel10) <*> (pure 0)
        output1 = ValidInt <$> output1Data <*> output1Aktv
        output1Data = getMatchingTag <$> out1 <*> ((.output1) <$> curTagsLevel10) <*> (pure 0)
        output2 = ValidInt <$> output2Data <*> output2Aktv
        output2Data = getMatchingTag <$> out2 <*> ((.output2) <$> curTagsLevel10) <*> (pure 0)
        output3 = ValidInt <$> output3Data <*> output3Aktv
        output3Data = getMatchingTag <$> out3 <*> ((.output3) <$> curTagsLevel10) <*> (pure 0)
        output4 = ValidInt <$> output4Data <*> output4Aktv
        output4Data = getMatchingTag <$> out4 <*> ((.output4) <$> curTagsLevel10) <*> (pure 0)
        output5 = ValidInt <$> output5Data <*> output5Aktv
        output5Data = getMatchingTag <$> out5 <*> ((.output5) <$> curTagsLevel10) <*> (pure 0)
        output6 = ValidInt <$> output6Data <*> output6Aktv
        (_, output6Data) = unbundle out6
        output7 = ValidInt <$> output7Data <*> output7Aktv
        (_, output7Data) = unbundle out7
        output8 = ValidInt <$> output8Data <*> output8Aktv
        (_, output8Data) = unbundle out8
        output9 = ValidInt <$> output9Data <*> output9Aktv
        (_, output9Data) = unbundle out9
        output10 = ValidInt <$> output10Data <*> output10Aktv
        (_, output10Data) = unbundle out10
        output11 = ValidInt <$> output11Data <*> output11Aktv
        (_, output11Data) = unbundle out11

        outputs = Outputs <$> output0 <*> output1 <*> output2 <*> output3 <*> output4 <*> output5 <*> output6 <*> output7 <*> output8 <*> output9 <*> output10 <*> output11

        debugSignals = debugEnables
        debugEnables = DebugEnables <$>
                            (getPacing <$> enIn0) <*>
                            (getPacing <$> enOut0) <*>
                            (getPacing <$> enOut1) <*>
                            (getPacing <$> enOut2) <*>
                            (getPacing <$> enOut3) <*>
                            (getPacing <$> enOut4) <*>
                            (getPacing <$> enOut5) <*>
                            (getPacing <$> enOut6) <*>
                            (getPacing <$> enOut7) <*>
                            (getPacing <$> enOut8) <*>
                            (getPacing <$> enOut9) <*>
                            (getPacing <$> enOut10) <*>
                            (getPacing <$> enOut11)

        genTag :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag
        genTag en = t
            where 
                t = register 1 (mux en next_t t)
                next_t = mux (t .==. (pure maxTag)) (pure 1) (t + 1)


pipelineReady :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
pipelineReady rst = toWait .==. pure 0 
    where 
        waitTime = pure 4 :: Signal dom Int
        toWait = register (0 :: Int) next
        next = mux rst waitTime (mux (toWait .>. pure 0) (toWait - 1) toWait)



input0Window :: HiddenClockResetEnable dom => Signal dom PacingIn0 -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
input0Window en tag val = result
    where result = register (invalidTag, 0) (mux (getPacing <$> en) (bundle (tag, val)) result)



outputStream0 :: HiddenClockResetEnable dom => Signal dom PacingOut0 -> Signal dom Tag -> Signal dom Int -> Signal dom Int -> Signal dom (Vec 2 (Tag, Int))
outputStream0 en tag in0_0 out2_1 = result
    where
        result = register (repeat (invalidTag, 0)) (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0_0 + out2_1


outputStream1 :: HiddenClockResetEnable dom => Signal dom PacingOut1 -> Signal dom Tag -> Signal dom Int -> Signal dom (Vec 2 (Tag, Int))
outputStream1 en tag out0_0 = result
    where
        result = register (repeat (invalidTag, 0)) (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out0_0 + 1


outputStream2 :: HiddenClockResetEnable dom => Signal dom PacingOut2 -> Signal dom Tag -> Signal dom Int -> Signal dom (Vec 2 (Tag, Int))
outputStream2 en tag out1_0 = result
    where
        result = register (repeat (invalidTag, 0)) (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out1_0 + 1


outputStream3 :: HiddenClockResetEnable dom => Signal dom PacingOut3 -> Signal dom Tag -> Signal dom Int -> Signal dom (Vec 2 (Tag, Int))
outputStream3 en tag out1_0 = result
    where
        result = register (repeat (invalidTag, 0)) (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out1_0 + 1


outputStream4 :: HiddenClockResetEnable dom => Signal dom PacingOut4 -> Signal dom Tag -> Signal dom Int -> Signal dom (Vec 2 (Tag, Int))
outputStream4 en tag out2_0 = result
    where
        result = register (repeat (invalidTag, 0)) (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out2_0 + 1


outputStream5 :: HiddenClockResetEnable dom => Signal dom PacingOut5 -> Signal dom Tag -> Signal dom Int -> Signal dom (Vec 2 (Tag, Int))
outputStream5 en tag out3_0 = result
    where
        result = register (repeat (invalidTag, 0)) (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out3_0 + 1


outputStream6 :: HiddenClockResetEnable dom => Signal dom PacingOut6 -> Signal dom Tag -> Signal dom Int -> Signal dom Int -> Signal dom (Tag, Int)
outputStream6 en tag out4_0 out11_1 = result
    where
        result = register (invalidTag, 0) (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out4_0 + out11_1


outputStream7 :: HiddenClockResetEnable dom => Signal dom PacingOut7 -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
outputStream7 en tag out5_0 = result
    where
        result = register (invalidTag, 0) (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out5_0 + 1


outputStream8 :: HiddenClockResetEnable dom => Signal dom PacingOut8 -> Signal dom Tag -> Signal dom Int -> Signal dom Int -> Signal dom (Tag, Int)
outputStream8 en tag out6_0 out10_1 = result
    where
        result = register (invalidTag, 0) (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out6_0 + out10_1


outputStream9 :: HiddenClockResetEnable dom => Signal dom PacingOut9 -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
outputStream9 en tag out7_0 = result
    where
        result = register (invalidTag, 0) (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out7_0 + 1


outputStream10 :: HiddenClockResetEnable dom => Signal dom PacingOut10 -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
outputStream10 en tag out9_0 = result
    where
        result = register (invalidTag, 0) (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out9_0 + 1


outputStream11 :: HiddenClockResetEnable dom => Signal dom PacingOut11 -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
outputStream11 en tag out8_0 = result
    where
        result = register (invalidTag, 0) (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out8_0 + 1







---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Outputs, (QPush, QPop, QPushValid, QPopValid, DebugEnables))
monitor inputs = bundle (outputs, debugSignals)
    where 
        (newEvent, event) = unbundle (hlc inputs)

        (qPushValid, qPopValid, qPopData) = unbundle (queue (bundle (qPush, qPop, qInptData)))
        qPush = newEvent
        qPop = toPop
        qInptData = event

        (llcOutput, llcDebug) = unbundle (llc (bundle (qPopValid, qPopData)))
        (toPop, outputs) = unbundle llcOutput

        debugEnables = llcDebug
        debugSignals = bundle (qPush, qPop, qPushValid, qPopValid, debugEnables)


---------------------------------------------------------------

topEntity :: Clock TestDomain -> Reset TestDomain -> Enable TestDomain -> 
    Signal TestDomain Inputs -> Signal TestDomain (Outputs, (QPush, QPop, QPushValid, QPopValid, DebugEnables))
topEntity clk rst en inputs = exposeClockResetEnable (monitor inputs) clk rst en
