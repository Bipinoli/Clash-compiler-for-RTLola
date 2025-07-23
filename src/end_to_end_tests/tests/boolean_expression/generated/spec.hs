{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Spec where

import Clash.Prelude

---------------------------------------------------------------

-- input x : Int
-- 
-- output a @2kHz := x.hold(or: 0)
-- output b := delta(a, dft: a) > 5
-- output c := delta(a, dft: a) < -5
-- output d := (b ∧ c.offset(by: -1).defaults(to: false)) ∨ (b.offset(by: -1).defaults(to: false) ∧ c)

---------------------------------------------------------------

-- Evaluation Order
--------------------
-- x
-- a
-- b, c
-- d

-- Memory Window
-----------------
-- window a = 3
-- window b = 2
-- window x = 1
-- window c = 2
-- window d = 1

-- Pipeline Visualization
--------------------------

-- x   | x   | x   | x   | x   | x   | x   | x   | x   | x  
-- ---------------------------------------------------------
--     | a   | a   | a   | a   | a   | a   | a   | a   | a  
-- ---------------------------------------------------------
--     |     | b,c | b,c | b,c | b,c | b,c | b,c | b,c | b,c
-- ---------------------------------------------------------
--     |     |     | d   | d   | d   | d   | d   | d   | d  
-- ---------------------------------------------------------

-- Nicknames
-------------
-- input0 = x
-- output0 = a
-- output1 = b
-- output2 = c
-- output3 = d

---------------------------------------------------------------

data ValidBool = ValidBool {
    value :: Bool,
    valid :: Bool
} deriving (Generic, NFDataX)

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
    output1 :: ValidBool,
    output2 :: ValidBool,
    output3 :: ValidBool
} deriving (Generic, NFDataX)


class Pacing a where getPacing :: a -> Bool

data PacingIn0 = PacingIn0 Bool deriving (Generic, NFDataX)
data PacingOut0 = PacingOut0 Bool deriving (Generic, NFDataX)
data PacingOut1 = PacingOut1 Bool deriving (Generic, NFDataX)
data PacingOut2 = PacingOut2 Bool deriving (Generic, NFDataX)
data PacingOut3 = PacingOut3 Bool deriving (Generic, NFDataX)

instance Pacing PacingIn0 where getPacing (PacingIn0 x) = x
instance Pacing PacingOut0 where getPacing (PacingOut0 x) = x
instance Pacing PacingOut1 where getPacing (PacingOut1 x) = x
instance Pacing PacingOut2 where getPacing (PacingOut2 x) = x
instance Pacing PacingOut3 where getPacing (PacingOut3 x) = x

data Pacings = Pacings {
    pacingIn0 :: PacingIn0,
    pacingOut0 :: PacingOut0,
    pacingOut1 :: PacingOut1,
    pacingOut2 :: PacingOut2,
    pacingOut3 :: PacingOut3
} deriving (Generic, NFDataX)

data DebugEnables = DebugEnables {
    enableIn0 :: Bool,
    enableOut0 :: Bool,
    enableOut1 :: Bool,
    enableOut2 :: Bool,
    enableOut3 :: Bool
} deriving (Generic, NFDataX)


type Tag = Unsigned 8

data Tags = Tags {
    input0 :: Tag,
    output0 :: Tag,
    output1 :: Tag,
    output2 :: Tag,
    output3 :: Tag
} deriving (Generic, NFDataX)

type Event = (Inputs, Pacings)

nullEvent :: Event
nullEvent = (nullInputs, nullPacings)
nullInputs = Inputs (ValidInt 0 False) 
nullPacings = Pacings 
                nullPacingIn0
                nullPacingOut0
                nullPacingOut1
                nullPacingOut2
                nullPacingOut3
nullPacingIn0 = PacingIn0 False
nullPacingOut0 = PacingOut0 False 
nullPacingOut1 = PacingOut1 False 
nullPacingOut2 = PacingOut2 False 
nullPacingOut3 = PacingOut3 False 


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

queue :: HiddenClockResetEnable dom 
    => Signal dom QInput 
    -> Signal dom QOutput
queue input = output
    where 
        output = bundle (pushValid, popValid, outData)
        state = bundle (buffer, cursor)
        buffer = register (repeat nullEvent :: QMem) nextBufferSignal
        cursor = register 0 nextCursorSignal
        pushValid = register False nextPushValidSignal
        popValid = register False nextPopValidSignal
        outData = register nullEvent nextOutDataSignal

        nextBufferSignal = nextBuffer  
                            <$> buffer 
                            <*> bundle (input, cursor)
        nextCursorSignal = nextCursor 
                            <$> cursor 
                            <*> bundle (input, buffer)
        nextOutDataSignal = nextOutData 
                            <$> bundle (input, cursor, buffer)
        nextPushValidSignal = nextPushValid 
                            <$> bundle (input, cursor, buffer)
        nextPopValidSignal = nextPopValid <$> bundle (input, cursor)
        
        nextBuffer :: QMem -> (QInput, QCursor) -> QMem
        nextBuffer buf ((push, pop, qData), cur) = out
            where 
                out = case (push, pop) of
                    (True, True) -> qData +>> buf 
                    (True, False) -> if cur == length buf 
                                    then buf else qData +>> buf
                    (False, _) -> buf

        nextCursor :: QCursor -> (QInput, QMem) -> QCursor
        nextCursor cur ((push, pop, _), buf) = out
            where 
                out = case (push, pop) of
                    (True, False) -> if cur == length buf 
                                    then cur else cur + 1
                    (False, True) -> if cur == 0 then 0 else cur - 1
                    (_, _) -> cur

        nextOutData :: (QInput, QCursor, QMem) -> QData
        nextOutData ((push, pop, qData), cur, buf) = out
            where 
                out = case (push, pop) of
                    (True, True) -> if cur == 0 
                                    then qData else buf !! (cur - 1)
                    (False, True) -> if cur == 0 
                                    then nullEvent else buf !! (cur - 1)
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
-- It has been arbitrarily chosen for both monitor 
--  and the verilog testbench simulation
createDomain vSystem{vName="TestDomain", vPeriod=2000} 
-- period in nanoseconds

systemClockPeriodNs :: Int
systemClockPeriodNs = fromInteger  
    (snatToInteger $ clockPeriod @TestDomain)


hlc :: HiddenClockResetEnable dom 
    => Signal dom Inputs 
    -> Signal dom (Bool, Event)
hlc inputs = out
    where 
        out = bundle (newEvent, event)
        newEvent = hasInput0 .||. timer0Over

        event = bundle (inputs, pacings)

        pacings = Pacings <$> pIn0 
                    <*> pOut0 
                    <*> pOut1 
                    <*> pOut2 
                    <*> pOut3

        hasInput0 = ((.valid). (.input0)) <$> inputs

        pIn0 = PacingIn0 <$> hasInput0
        pOut0 = PacingOut0 <$> timer0Over
        pOut1 = PacingOut1 <$> timer0Over
        pOut2 = PacingOut2 <$> timer0Over
        pOut3 = PacingOut3 <$> timer0Over


        timer0Over = timer0 .>=. period0InNs
        timer0 = timer timer0Over
        period0InNs = 500000

        timer :: HiddenClockResetEnable dom 
            => Signal dom Bool 
            -> Signal dom Int
        timer reset = register 0 (mux reset (pure deltaTime) nextTime)
            where 
                nextTime = timer reset + pure deltaTime
                deltaTime = systemClockPeriodNs


---------------------------------------------------------------

-- To avoid duplidate tags in a window 
-- maxTag must be at least the size of the maximum window 
-- Also to avoid having to do modulo operations 
-- maxTag must be at least as big as the largest offset
maxTag = 4 :: Tag
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
getMatchingTagFromNonVec (tag, dta) tagToMatch dflt = 
    if tag == tagToMatch then dta else dflt

getLatestValue :: KnownNat n => Vec (n + 1) (Tag, a) -> a -> a
getLatestValue win dflt =
    let (tag, dta) = last win
    in if tag == invalidTag then dflt else dta

getLatestValueFromNonVec :: (Tag, a) -> a -> a
getLatestValueFromNonVec (tag, dta) dflt = 
    if tag == invalidTag then dflt else dta

earlierTag :: Tag -> Tag -> Tag
earlierTag curTag cyclesBefore = 
    if curTag > cyclesBefore 
    then curTag - cyclesBefore 
    else curTag - cyclesBefore + maxTag

delayFor :: forall dom n a . 
    (HiddenClockResetEnable dom, KnownNat n, NFDataX a)
    => SNat n
    -> a
    -> Signal dom a
    -> Signal dom a
delayFor n initVal sig = last delayedVec
    where
        delayedVec :: Vec (n + 1) (Signal dom a)
        delayedVec = iterateI (delay initVal) sig
    

llc :: HiddenClockResetEnable dom 
    => Signal dom (Bool, Event) 
    -> Signal dom ((Bool, Outputs), (DebugEnables))
llc event = bundle (bundle (toPop, outputs), debugSignals)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        toPop = pure True

        (inputs, pacings) = unbundle poppedEvent

        input0 = (.input0) <$> inputs


        pIn0 = (.pacingIn0) <$> pacings
        pOut0 = (.pacingOut0) <$> pacings
        pOut1 = (.pacingOut1) <$> pacings
        pOut2 = (.pacingOut2) <$> pacings
        pOut3 = (.pacingOut3) <$> pacings
        
        tIn0 = genTag (getPacing <$> pIn0)
        tOut0 = genTag (getPacing <$> pOut0)
        tOut1 = genTag (getPacing <$> pOut1)
        tOut2 = genTag (getPacing <$> pOut2)
        tOut3 = genTag (getPacing <$> pOut3)

        -- tag generation takes 1 cycle so we need to delay the input
        input0Data = delay 0 (((.value). (.input0)) <$> inputs)

        -- delayed tags to be used in different levels 
        tagsDefault = Tags 
                nullT 
                nullT 
                nullT 
                nullT 
                nullT 
        curTags = Tags 
                <$> tIn0 
                <*> tOut0 
                <*> tOut1 
                <*> tOut2 
                <*> tOut3
        curTagsLevel0 = curTags
        curTagsLevel1 = delayFor d1 tagsDefault curTags
        curTagsLevel2 = delayFor d2 tagsDefault curTags
        curTagsLevel3 = delayFor d3 tagsDefault curTags
        curTagsLevel4 = delayFor d4 tagsDefault curTags
        nullT = invalidTag

        enIn0 = delayFor d1 nullPacingIn0 pIn0
        enOut0 = delayFor d2 nullPacingOut0 pOut0
        enOut1 = delayFor d3 nullPacingOut1 pOut1
        enOut2 = delayFor d3 nullPacingOut2 pOut2
        enOut3 = delayFor d4 nullPacingOut3 pOut3

        output0Aktv = delayFor d5 False (getPacing <$> pOut0)
        output1Aktv = delayFor d5 False (getPacing <$> pOut1)
        output2Aktv = delayFor d5 False (getPacing <$> pOut2)
        output3Aktv = delayFor d5 False (getPacing <$> pOut3)

        -- Evaluation of input windows: level 0
        input0Win = input0Window enIn0 tIn0 input0Data

        -- Evaluation of output 0: level 1
        out0 = outputStream0 enOut0 
            ((.output0) <$> curTagsLevel1) 
            out0Data0 
        out0Data0 = getLatestValueFromNonVec 
            <$> input0Win 
            <*> out0Data0Dflt
        out0Data0Dflt = pure (0)

        -- Evaluation of output 1: level 2
        out1 = outputStream1 enOut1 
            ((.output1) <$> curTagsLevel2) 
            out1Data0 
            out1Data1 
        out1Data0 = getMatchingTag 
            <$> out0 
            <*> ((.output0) <$> curTagsLevel2) 
            <*> (pure (0))
        out1Data1 = getOffset         
            <$> out0 
            <*> ((.output0) <$> curTagsLevel2) 
            <*> (pure 1) 
            <*> out1Data1Dflt
        out1Data1Dflt =  getMatchingTag <$> out0 <*> ((.output0) <$> curTagsLevel2) <*> (pure (0))

        -- Evaluation of output 2: level 2
        out2 = outputStream2 enOut2 
            ((.output2) <$> curTagsLevel2) 
            out2Data0 
            out2Data1 
        out2Data0 = getMatchingTag 
            <$> out0 
            <*> ((.output0) <$> curTagsLevel2) 
            <*> (pure (0))
        out2Data1 = getOffset         
            <$> out0 
            <*> ((.output0) <$> curTagsLevel2) 
            <*> (pure 1) 
            <*> out2Data1Dflt
        out2Data1Dflt =  getMatchingTag <$> out0 <*> ((.output0) <$> curTagsLevel2) <*> (pure (0))

        -- Evaluation of output 3: level 3
        out3 = outputStream3 enOut3 
            ((.output3) <$> curTagsLevel3) 
            out3Data0 
            out3Data1 
            out3Data2 
            out3Data3 
        out3Data0 = getMatchingTag 
            <$> out1 
            <*> ((.output1) <$> curTagsLevel3) 
            <*> (pure (False))
        out3Data1 = getOffset         
            <$> out2 
            <*> ((.output2) <$> curTagsLevel3) 
            <*> (pure 1) 
            <*> out3Data1Dflt
        out3Data1Dflt = pure (False)
        out3Data2 = getOffset         
            <$> out1 
            <*> ((.output1) <$> curTagsLevel3) 
            <*> (pure 1) 
            <*> out3Data2Dflt
        out3Data2Dflt = pure (False)
        out3Data3 = getMatchingTag 
            <$> out2 
            <*> ((.output2) <$> curTagsLevel3) 
            <*> (pure (False))

        -- Outputing all results: level 4
        output0 = ValidInt <$> output0Data <*> output0Aktv
        output0Data = getMatchingTag 
            <$> out0 
            <*> ((.output0) 
            <$> curTagsLevel4) 
            <*> (pure 0)
        output1 = ValidBool <$> output1Data <*> output1Aktv
        output1Data = getMatchingTag 
            <$> out1 
            <*> ((.output1) 
            <$> curTagsLevel4) 
            <*> (pure False)
        output2 = ValidBool <$> output2Data <*> output2Aktv
        output2Data = getMatchingTag 
            <$> out2 
            <*> ((.output2) 
            <$> curTagsLevel4) 
            <*> (pure False)
        output3 = ValidBool <$> output3Data <*> output3Aktv
        (_, output3Data) = unbundle out3

        outputs = Outputs 
            <$> output0 
            <*> output1 
            <*> output2 
            <*> output3

        debugSignals = debugEnables
        debugEnables = DebugEnables <$>
                            (getPacing <$> enIn0) <*>
                            (getPacing <$> enOut0) <*>
                            (getPacing <$> enOut1) <*>
                            (getPacing <$> enOut2) <*>
                            (getPacing <$> enOut3)

        genTag :: HiddenClockResetEnable dom 
            => Signal dom Bool 
            -> Signal dom Tag
        genTag en = t
            where 
                t = register 1 (mux en next_t t)
                next_t = mux (t .==. (pure maxTag)) (pure 1) (t + 1)





input0Window :: HiddenClockResetEnable dom 
    => Signal dom PacingIn0 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Tag, Int)
input0Window en tag val = result
    where result = register (invalidTag, 0) 
                    (mux (getPacing <$> en) (bundle (tag, val)) result)



outputStream0 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut0 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Vec 3 (Tag, Int))
outputStream0 en tag in0_ = result
    where
        result = register (repeat (invalidTag, 0)) 
                (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0_


outputStream1 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut1 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom Int 
    -> Signal dom (Vec 2 (Tag, Bool))
outputStream1 en tag out0_00 out0_01 = result
    where
        result = register (repeat (invalidTag, False)) 
                (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = ((out0_00 - out0_01) .>. (5))


outputStream2 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut2 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom Int 
    -> Signal dom (Vec 2 (Tag, Bool))
outputStream2 en tag out0_00 out0_01 = result
    where
        result = register (repeat (invalidTag, False)) 
                (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = ((out0_00 - out0_01) .<. (-5))


outputStream3 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut3 
    -> Signal dom Tag 
    -> Signal dom Bool 
    -> Signal dom Bool 
    -> Signal dom Bool 
    -> Signal dom Bool 
    -> Signal dom (Tag, Bool)
outputStream3 en tag out1_00 out2_01 out1_10 out2_11 = result
    where
        result = register (invalidTag, False) 
                (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = ((out1_00 .&&. out2_01) .||. (out1_10 .&&. out2_11))







---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom 
    => Signal dom Inputs 
    ->  Signal dom (Outputs, (QPush, QPop, QPushValid, QPopValid, DebugEnables))
monitor inputs = bundle (outputs, debugSignals)
    where 
        (newEvent, event) = unbundle (hlc inputs)

        (qPushValid, qPopValid, qPopData) = 
            unbundle (queue (bundle (qPush, qPop, qInptData)))
        qPush = newEvent
        qPop = toPop
        qInptData = event

        (llcOutput, llcDebug) = unbundle (llc (bundle (qPopValid, qPopData)))
        (toPop, outputs) = unbundle llcOutput

        debugEnables = llcDebug
        debugSignals = bundle (qPush, qPop, qPushValid, qPopValid, debugEnables)


---------------------------------------------------------------

topEntity :: Clock TestDomain 
    -> Reset TestDomain 
    -> Enable TestDomain -> Signal TestDomain Inputs 
    -> Signal TestDomain (Outputs, (QPush, QPop, QPushValid, QPopValid, DebugEnables))
topEntity clk rst en inputs = 
    exposeClockResetEnable (monitor inputs) clk rst en
