{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Spec where

import Clash.Prelude

---------------------------------------------------------------

-- input x : Int
-- input y : Int
-- 
-- output a := x.offset(by: -1).defaults(to: c)
-- output b := a.offset(by: -1).defaults(to: a) + y.offset(by: -1).defaults(to: c + x + y)
-- output c := y.offset(by: -1).defaults(to: y)
-- output d := x.offset(by: -1).defaults(to: a) + b

---------------------------------------------------------------

-- Evaluation Order
-- x, y
-- c
-- a
-- b
-- d

-- Memory Window
-- window d = 1
-- window x = 5
-- window a = 3
-- window c = 4
-- window y = 4
-- window b = 2

-- Pipeline Visualization
-- x,y | x,y | x,y | x,y | x,y | x,y | x,y | x,y | x,y | x,y
-- ---------------------------------------------------------
--     | c   | c   | c   | c   | c   | c   | c   | c   | c  
-- ---------------------------------------------------------
--     |     | a   | a   | a   | a   | a   | a   | a   | a  
-- ---------------------------------------------------------
--     |     |     | b   | b   | b   | b   | b   | b   | b  
-- ---------------------------------------------------------
--     |     |     |     | d   | d   | d   | d   | d   | d  
-- ---------------------------------------------------------

-- input0 = x
-- input1 = y
-- output0 = a
-- output1 = b
-- output2 = c
-- output3 = d

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
    output3 :: ValidInt
} deriving (Generic, NFDataX)

data Pacings = Pacings {
    pacing0 :: Bool,
    pacing1 :: Bool,
    pacing2 :: Bool,
    pacing3 :: Bool
} deriving (Generic, NFDataX)


type Tag = Unsigned 8

data Tags = Tags {
    input0 :: Tag,
    input1 :: Tag,
    output0 :: Tag,
    output1 :: Tag,
    output2 :: Tag,
    output3 :: Tag
} deriving (Generic, NFDataX)

type Event = (Inputs, Pacings)

nullEvent :: Event
nullEvent = (nullInputs, nullPacings)
nullInputs = Inputs (ValidInt 0 False) (ValidInt 0 False) 
nullPacings = Pacings False False False False 

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
        newEvent = hasInput0 .||. hasInput1

        event = bundle (inputs, pacings)

        pacings = Pacings <$> p0 <*> p1 <*> p2 <*> p3

        hasInput0 = ((.valid). (.input0)) <$> inputs
        hasInput1 = ((.valid). (.input1)) <$> inputs

        p0 = hasInput0 .&&. hasInput1
        p1 = hasInput0 .&&. hasInput1
        p2 = hasInput1
        p3 = hasInput0 .&&. hasInput1





---------------------------------------------------------------

-- maxTag must be at least the size of the maximum window to avoid duplicate tags in the window
-- also to avoid having to do modulo operations maxTag must be at least as big as the largest offset
maxTag = 6 :: Tag
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
     

llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom ((Bool, Outputs), (Pacings))
llc event = bundle (bundle (toPop, outputs), debugSignals)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        toPop = pure True

        (inputs, pacings) = unbundle poppedEvent

        input0 = (.input0) <$> inputs
        input1 = (.input1) <$> inputs
        input0HasData = ((.valid). (.input0)) <$> inputs
        input1HasData = ((.valid). (.input1)) <$> inputs


        p0 = (.pacing0) <$> pacings
        p1 = (.pacing1) <$> pacings
        p2 = (.pacing2) <$> pacings
        p3 = (.pacing3) <$> pacings
        
        tIn0 = genTag input0HasData
        tIn1 = genTag input1HasData
        tOut2 = genTag p2
        tOut0 = genTag p0
        tOut1 = genTag p1
        tOut3 = genTag p3

        -- tag generation takes 1 cycle so we need to delay the input data
        input0Data = delay 0 (((.value). (.input0)) <$> inputs)
        input1Data = delay 0 (((.value). (.input1)) <$> inputs)

        -- delayed tags to be used in different levels 
        tagsDefault = Tags nullT nullT nullT nullT nullT nullT 
        curTags = Tags <$> tIn0 <*> tIn1 <*> tOut0 <*> tOut1 <*> tOut2 <*> tOut3
        curTagsLevel1 = delayFor d1 tagsDefault curTags
        curTagsLevel2 = delayFor d2 tagsDefault curTags
        curTagsLevel3 = delayFor d3 tagsDefault curTags
        curTagsLevel4 = delayFor d4 tagsDefault curTags
        curTagsLevel5 = delayFor d5 tagsDefault curTags
        nullT = invalidTag

        enIn0 = delayFor d1 False input0HasData
        enIn1 = delayFor d1 False input1HasData
        enOut2 = delayFor d2 False p2
        enOut0 = delayFor d3 False p0
        enOut1 = delayFor d4 False p1
        enOut3 = delayFor d5 False p3

        output0Aktv = delayFor d6 False p0
        output1Aktv = delayFor d6 False p1
        output2Aktv = delayFor d6 False p2
        output3Aktv = delayFor d6 False p3

        -- Evaluation of input windows: level 0
        input0Win = input0Window enIn0 tIn0 input0Data
        input1Win = input1Window enIn1 tIn1 input1Data

        -- Evaluation of output 0: level 2
        out0 = outputStream0 enOut0 ((.output0) <$> curTagsLevel2) out0Data0 
        out0Data0 = getOffset <$> input0Win <*> ((.input0) <$> curTagsLevel2) <*> (pure 1) <*> out0Data0Dflt
        (_, out0Data0Dflt) = unbundle (getMatchingTag <$> out2 <*> ((.output2) <$> curTagsLevel2) <*> (pure (0)))

        -- Evaluation of output 1: level 3
        out1 = outputStream1 enOut1 ((.output1) <$> curTagsLevel3) out1Data0 out1Data1 
        out1Data0 = getOffset <$> input1Win <*> ((.input1) <$> curTagsLevel3) <*> (pure 1) <*> out1Data0Dflt
        out1Data0Dflt = out1Data0DfltData0 + out1Data0DfltData1
        out1Data0DfltData0 = out1Data0DfltData0Data0 + out1Data0DfltData0Data1
        (_, out1Data0DfltData0Data0) = unbundle (getMatchingTag <$> out2 <*> ((.output2) <$> curTagsLevel3) <*> (pure (0)))
        (_, out1Data0DfltData0Data1) = unbundle (getMatchingTag <$> input0Win <*> ((.input0) <$> curTagsLevel3) <*> (pure (0)))
        (_, out1Data0DfltData1) = unbundle (getMatchingTag <$> input1Win <*> ((.input1) <$> curTagsLevel3) <*> (pure (0)))
        out1Data1 = getOffset <$> out0 <*> ((.output0) <$> curTagsLevel3) <*> (pure 1) <*> out1Data1Dflt
        (_, out1Data1Dflt) = unbundle (getMatchingTag <$> out0 <*> ((.output0) <$> curTagsLevel3) <*> (pure (0)))

        -- Evaluation of output 2: level 1
        out2 = outputStream2 enOut2 ((.output2) <$> curTagsLevel1) out2Data0 
        out2Data0 = getOffset <$> input1Win <*> ((.input1) <$> curTagsLevel1) <*> (pure 1) <*> out2Data0Dflt
        (_, out2Data0Dflt) = unbundle (getMatchingTag <$> input1Win <*> ((.input1) <$> curTagsLevel1) <*> (pure (0)))

        -- Evaluation of output 3: level 4
        out3 = outputStream3 enOut3 ((.output3) <$> curTagsLevel4) out3Data0 out3Data1 
        out3Data0 = getOffset <$> input0Win <*> ((.input0) <$> curTagsLevel4) <*> (pure 1) <*> out3Data0Dflt
        (_, out3Data0Dflt) = unbundle (getMatchingTag <$> out0 <*> ((.output0) <$> curTagsLevel4) <*> (pure (0)))
        out3Data1 = getMatchingTag <$> out1 <*> ((.output1) <$> curTagsLevel4) <*> (pure (0))

        -- Outputing all results: level 5
        output0 = ValidInt <$> output0Data <*> output0Aktv
        (_, output0Data) = unbundle (getMatchingTag <$> out0 <*> ((.output0) <$> curTagsLevel5) <*> (pure 0))
        output1 = ValidInt <$> output1Data <*> output1Aktv
        (_, output1Data) = unbundle (getMatchingTag <$> out1 <*> ((.output1) <$> curTagsLevel5) <*> (pure 0))
        output2 = ValidInt <$> output2Data <*> output2Aktv
        (_, output2Data) = unbundle (getMatchingTag <$> out2 <*> ((.output2) <$> curTagsLevel5) <*> (pure 0))
        output3 = ValidInt <$> output3Data <*> output3Aktv
        (_, output3Data) = unbundle out3

        outputs = Outputs <$> output0 <*> output1 <*> output2 <*> output3

        debugSignals = pacings

        genTag :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag
        genTag en = t
            where 
                t = register 1 (mux en next_t t)
                next_t = mux (t .==. (pure maxTag)) (pure 1) (t + 1)





input0Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Vec 5 (Tag, Int))
input0Window en tag val = result
    where result = register (repeat (invalidTag, 0)) (mux en ((<<+) <$> result <*> (bundle (tag, val))) result)


input1Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Vec 4 (Tag, Int))
input1Window en tag val = result
    where result = register (repeat (invalidTag, 0)) (mux en ((<<+) <$> result <*> (bundle (tag, val))) result)



outputStream0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Vec 3 (Tag, Int))
outputStream0 en tag in0WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0
        (_, in0) = unbundle in0WithTag


outputStream1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
outputStream1 en tag in1WithTag out0WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out0 + in1
        (_, in1) = unbundle in1WithTag
        (_, out0) = unbundle out0WithTag


outputStream2 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Vec 4 (Tag, Int))
outputStream2 en tag in1WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in1
        (_, in1) = unbundle in1WithTag


outputStream3 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream3 en tag in0WithTag out1WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + out1
        (_, in0) = unbundle in0WithTag
        (_, out1) = unbundle out1WithTag







---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Outputs, (QPush, QPop, QPushValid, QPopValid, Pacings))
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
    Signal TestDomain Inputs -> Signal TestDomain (Outputs, (QPush, QPop, QPushValid, QPopValid, Pacings))
topEntity clk rst en inputs = exposeClockResetEnable (monitor inputs) clk rst en
