{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Spec where

import Clash.Prelude

---------------------------------------------------------------

-- input gps_x : Int
-- input num_satellites: Int
-- input imu_acc_x: Int 
-- 
-- output gps_emitted_enough @1Hz := gps_x.aggregate(over: 3s, using: count) < 10
-- output few_satellites := num_satellites < 9
-- output is_unreliable_gps_data @1Hz := 
--     few_satellites.aggregate(over: 5s, using: count) > 12
-- 

---------------------------------------------------------------

-- Evaluation Order
--------------------
-- num_satellites, imu_acc_x, gps_x
-- few_satellites
-- sw(few_satellites,is_unreliable_gps_data), sw(gps_x,gps_emitted_enough)
-- is_unreliable_gps_data, gps_emitted_enough

-- Memory Window
-----------------
-- window gps_emitted_enough = 1
-- window few_satellites = 3
-- window sw(gps_x,gps_emitted_enough) = 1
-- window is_unreliable_gps_data = 1
-- window gps_x = 2
-- window sw(few_satellites,is_unreliable_gps_data) = 1
-- window imu_acc_x = 1
-- window num_satellites = 1

-- Pipeline Visualization
--------------------------

-- num_satellites,imu_acc_x,gps_x                                         | num_satellites,imu_acc_x,gps_x                                         | num_satellites,imu_acc_x,gps_x                                         | num_satellites,imu_acc_x,gps_x                                         | num_satellites,imu_acc_x,gps_x                                         | num_satellites,imu_acc_x,gps_x                                         | num_satellites,imu_acc_x,gps_x                                         | num_satellites,imu_acc_x,gps_x                                         | num_satellites,imu_acc_x,gps_x                                         | num_satellites,imu_acc_x,gps_x                                        
-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--                                                                        | few_satellites                                                         | few_satellites                                                         | few_satellites                                                         | few_satellites                                                         | few_satellites                                                         | few_satellites                                                         | few_satellites                                                         | few_satellites                                                         | few_satellites                                                        
-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--                                                                        |                                                                        | sw(few_satellites,is_unreliable_gps_data),sw(gps_x,gps_emitted_enough) | sw(few_satellites,is_unreliable_gps_data),sw(gps_x,gps_emitted_enough) | sw(few_satellites,is_unreliable_gps_data),sw(gps_x,gps_emitted_enough) | sw(few_satellites,is_unreliable_gps_data),sw(gps_x,gps_emitted_enough) | sw(few_satellites,is_unreliable_gps_data),sw(gps_x,gps_emitted_enough) | sw(few_satellites,is_unreliable_gps_data),sw(gps_x,gps_emitted_enough) | sw(few_satellites,is_unreliable_gps_data),sw(gps_x,gps_emitted_enough) | sw(few_satellites,is_unreliable_gps_data),sw(gps_x,gps_emitted_enough)
-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--                                                                        |                                                                        |                                                                        | is_unreliable_gps_data,gps_emitted_enough                              | is_unreliable_gps_data,gps_emitted_enough                              | is_unreliable_gps_data,gps_emitted_enough                              | is_unreliable_gps_data,gps_emitted_enough                              | is_unreliable_gps_data,gps_emitted_enough                              | is_unreliable_gps_data,gps_emitted_enough                              | is_unreliable_gps_data,gps_emitted_enough                             
-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Nicknames
-------------
-- input0 = gps_x
-- input1 = num_satellites
-- input2 = imu_acc_x
-- output0 = gps_emitted_enough
-- output1 = few_satellites
-- output2 = is_unreliable_gps_data
-- sw0 = sw(gps_x,gps_emitted_enough)
-- sw1 = sw(few_satellites,is_unreliable_gps_data)

---------------------------------------------------------------

data ValidBool = ValidBool {
    value :: Bool,
    valid :: Bool
} deriving (Generic, NFDataX)

data ValidInt = ValidInt {
    value :: Int,
    valid :: Bool
} deriving (Generic, NFDataX)


data Inputs = Inputs {
    input0 :: ValidInt,
    input1 :: ValidInt,
    input2 :: ValidInt
} deriving (Generic, NFDataX)

data Outputs = Outputs {
    output0 :: ValidBool,
    output1 :: ValidBool,
    output2 :: ValidBool
} deriving (Generic, NFDataX)


class Pacing a where getPacing :: a -> Bool

data PacingIn0 = PacingIn0 Bool deriving (Generic, NFDataX)
data PacingIn1 = PacingIn1 Bool deriving (Generic, NFDataX)
data PacingIn2 = PacingIn2 Bool deriving (Generic, NFDataX)
data PacingOut0 = PacingOut0 Bool deriving (Generic, NFDataX)
data PacingOut1 = PacingOut1 PacingIn1 deriving (Generic, NFDataX)
data PacingOut2 = PacingOut2 Bool deriving (Generic, NFDataX)

instance Pacing PacingIn0 where getPacing (PacingIn0 x) = x
instance Pacing PacingIn1 where getPacing (PacingIn1 x) = x
instance Pacing PacingIn2 where getPacing (PacingIn2 x) = x
instance Pacing PacingOut0 where getPacing (PacingOut0 x) = x
instance Pacing PacingOut1 where getPacing (PacingOut1 x) = getPacing x
instance Pacing PacingOut2 where getPacing (PacingOut2 x) = x

data Pacings = Pacings {
    pacingIn0 :: PacingIn0,
    pacingIn1 :: PacingIn1,
    pacingIn2 :: PacingIn2,
    pacingOut0 :: PacingOut0,
    pacingOut1 :: PacingOut1,
    pacingOut2 :: PacingOut2
} deriving (Generic, NFDataX)

data Slides = Slides {
    slide0 :: Bool,
    slide1 :: Bool
} deriving (Generic, NFDataX)

type Tag = Unsigned 8

data Tags = Tags {
    input0 :: Tag,
    input1 :: Tag,
    input2 :: Tag,
    output0 :: Tag,
    output1 :: Tag,
    output2 :: Tag,
    slide0 :: Tag,
    slide1 :: Tag
} deriving (Generic, NFDataX)

type Event = (Inputs, Slides, Pacings)

nullEvent :: Event
nullEvent = (nullInputs, nullSlides, nullPacings)
nullInputs = Inputs (ValidInt 0 False) (ValidInt 0 False) (ValidInt 0 False) 
nullSlides = Slides False False 
nullPacings = Pacings 
                nullPacingIn0
                nullPacingIn1
                nullPacingIn2
                nullPacingOut0
                nullPacingOut1
                nullPacingOut2
nullPacingIn0 = PacingIn0 False
nullPacingIn1 = PacingIn1 False
nullPacingIn2 = PacingIn2 False
nullPacingOut0 = PacingOut0 False 
nullPacingOut1 = PacingOut1 nullPacingIn1 
nullPacingOut2 = PacingOut2 False 


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
        newEvent = hasInput0 .||. hasInput1 .||. hasInput2 .||. timer0Over

        event = bundle (inputs, slides, pacings)

        slides = Slides <$> s0 
                    <*> s1
        pacings = Pacings <$> pIn0 
                    <*> pIn1 
                    <*> pIn2 
                    <*> pOut0 
                    <*> pOut1 
                    <*> pOut2

        hasInput0 = ((.valid). (.input0)) <$> inputs
        hasInput1 = ((.valid). (.input1)) <$> inputs
        hasInput2 = ((.valid). (.input2)) <$> inputs

        pIn0 = PacingIn0 <$> hasInput0
        pIn1 = PacingIn1 <$> hasInput1
        pIn2 = PacingIn2 <$> hasInput2
        pOut0 = PacingOut0 <$> timer0Over
        pOut1 = PacingOut1 <$> pIn1
        pOut2 = PacingOut2 <$> timer0Over

        s0 = timer0Over
        s1 = timer0Over

        timer0Over = timer0 .>=. period0InNs
        timer0 = timer timer0Over
        period0InNs = 1000000000

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
maxTag = 7 :: Tag
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
    -> Signal dom (Bool, Outputs)
llc event = bundle (toPop, outputs)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        toPop = pure True

        (inputs, slides, pacings) = unbundle poppedEvent

        input0 = (.input0) <$> inputs
        input1 = (.input1) <$> inputs
        input2 = (.input2) <$> inputs

        slide0 = (.slide0) <$> slides
        slide1 = (.slide1) <$> slides

        pIn0 = (.pacingIn0) <$> pacings
        pIn1 = (.pacingIn1) <$> pacings
        pIn2 = (.pacingIn2) <$> pacings
        pOut0 = (.pacingOut0) <$> pacings
        pOut1 = (.pacingOut1) <$> pacings
        pOut2 = (.pacingOut2) <$> pacings
        
        tIn1 = genTag (getPacing <$> pIn1)
        tIn2 = genTag (getPacing <$> pIn2)
        tIn0 = genTag (getPacing <$> pIn0)
        tOut1 = genTag (getPacing <$> pOut1)
        tSw1 = genTag (getPacing <$> pOut1)
        tSw0 = genTag (getPacing <$> pIn0)
        tOut2 = genTag (getPacing <$> pOut2)
        tOut0 = genTag (getPacing <$> pOut0)

        -- tag generation takes 1 cycle so we need to delay the input
        input0Data = delay 0 (((.value). (.input0)) <$> inputs)
        input1Data = delay 0 (((.value). (.input1)) <$> inputs)
        input2Data = delay 0 (((.value). (.input2)) <$> inputs)

        -- delayed tags to be used in different levels 
        tagsDefault = Tags 
                nullT 
                nullT 
                nullT 
                nullT 
                nullT 
                nullT 
                nullT 
                nullT 
        curTags = Tags 
                <$> tIn0 
                <*> tIn1 
                <*> tIn2 
                <*> tOut0 
                <*> tOut1 
                <*> tOut2 
                <*> tSw0 
                <*> tSw1
        curTagsLevel1 = delayFor d1 tagsDefault curTags
        curTagsLevel2 = delayFor d2 tagsDefault curTags
        curTagsLevel3 = delayFor d3 tagsDefault curTags
        curTagsLevel4 = delayFor d4 tagsDefault curTags
        nullT = invalidTag

        enIn1 = delayFor d1 nullPacingIn1 pIn1
        enIn2 = delayFor d1 nullPacingIn2 pIn2
        enIn0 = delayFor d1 nullPacingIn0 pIn0
        enOut1 = delayFor d2 nullPacingOut1 pOut1
        enSw1 = delayFor d3 nullPacingOut1 pOut1
        sld1 = delayFor d3 False slide1
        enSw0 = delayFor d3 nullPacingIn0 pIn0
        sld0 = delayFor d3 False slide0
        enOut2 = delayFor d4 nullPacingOut2 pOut2
        enOut0 = delayFor d4 nullPacingOut0 pOut0

        output0Aktv = delayFor d5 False (getPacing <$> pOut0)
        output1Aktv = delayFor d5 False (getPacing <$> pOut1)
        output2Aktv = delayFor d5 False (getPacing <$> pOut2)

        -- Evaluation of input windows: level 0
        input0Win = input0Window enIn0 tIn0 input0Data
        input1Win = input1Window enIn1 tIn1 input1Data
        input2Win = input2Window enIn2 tIn2 input2Data

        -- Evaluation of output 0: level 3
        out0 = outputStream0 enOut0 
            ((.output0) <$> curTagsLevel3) 
            out0Data0 
        (_, out0Data0) = unbundle sw0

        -- Evaluation of output 1: level 1
        out1 = outputStream1 enOut1 
            ((.output1) <$> curTagsLevel1) 
            out1Data0 
        out1Data0 = getMatchingTagFromNonVec 
            <$> input1Win 
            <*> ((.input1) <$> curTagsLevel1) 
            <*> (pure (0))

        -- Evaluation of output 2: level 3
        out2 = outputStream2 enOut2 
            ((.output2) <$> curTagsLevel3) 
            out2Data0 
        (_, out2Data0) = unbundle sw1

        -- Evaluation of sliding window 0: level 2
        sw0 = slidingWindow0 enSw0 sld0 
            ((.slide0) <$> curTagsLevel2) sw0Data
        sw0Data = getMatchingTag 
            <$> input0Win 
            <*> ((.input0) <$> curTagsLevel2) 
            <*> (pure 0)

        -- Evaluation of sliding window 1: level 2
        sw1 = slidingWindow1 enSw1 sld1 
            ((.slide1) <$> curTagsLevel2) sw1Data
        sw1Data = getMatchingTag 
            <$> out1 
            <*> ((.output1) <$> curTagsLevel2) 
            <*> (pure False)

        -- Outputing all results: level 4
        output0 = ValidBool <$> output0Data <*> output0Aktv
        (_, output0Data) = unbundle out0
        output1 = ValidBool <$> output1Data <*> output1Aktv
        output1Data = getMatchingTag 
            <$> out1 
            <*> ((.output1) 
            <$> curTagsLevel4) 
            <*> (pure False)
        output2 = ValidBool <$> output2Data <*> output2Aktv
        (_, output2Data) = unbundle out2

        outputs = Outputs 
            <$> output0 
            <*> output1 
            <*> output2


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
    -> Signal dom (Vec 2 (Tag, Int))
input0Window en tag val = result
    where result = register (repeat (invalidTag, 0)) 
                    (mux (getPacing <$> en) 
                        ((<<+) <$> result <*> (bundle (tag, val)))
                        result)


input1Window :: HiddenClockResetEnable dom 
    => Signal dom PacingIn1 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Tag, Int)
input1Window en tag val = result
    where result = register (invalidTag, 0) 
                    (mux (getPacing <$> en) (bundle (tag, val)) result)


input2Window :: HiddenClockResetEnable dom 
    => Signal dom PacingIn2 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Tag, Int)
input2Window en tag val = result
    where result = register (invalidTag, 0) 
                    (mux (getPacing <$> en) (bundle (tag, val)) result)



outputStream0 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut0 
    -> Signal dom Tag 
    -> Signal dom (Vec 4 Int) 
    -> Signal dom (Tag, Bool)
outputStream0 en tag sw0 = result
    where
        result = register (invalidTag, False) 
                (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = ((merge0 <$> sw0) .<. (10))
        merge0 :: Vec 4 Int -> Int
        merge0 win = fold windowFunc2 (tail win)


outputStream1 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut1 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Vec 3 (Tag, Bool))
outputStream1 en tag in1_0 = result
    where
        result = register (repeat (invalidTag, False)) 
                (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = (in1_0 .<. (9))


outputStream2 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut2 
    -> Signal dom Tag 
    -> Signal dom (Vec 6 Int) 
    -> Signal dom (Tag, Bool)
outputStream2 en tag sw1 = result
    where
        result = register (invalidTag, False) 
                (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = ((merge1 <$> sw1) .>. (12))
        merge1 :: Vec 6 Int -> Int
        merge1 win = fold windowFunc2 (tail win)



windowFunc0 :: Int -> Bool -> Int
windowFunc0 acc item = acc + 1

windowFunc1 :: Int -> Int -> Int
windowFunc1 acc item = acc + 1

windowFunc2 :: Int -> Int -> Int
windowFunc2 acc item = acc + item


slidingWindow0 :: HiddenClockResetEnable dom 
    => Signal dom PacingIn0 
    -> Signal dom Bool 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Tag, (Vec 4 Int)) 
slidingWindow0 newData slide tag inpt = window
    where
        window = register (invalidTag, dflt) (mux en next window)
        next = bundle (tag, nextWindow <$> (snd <$> window) 
                <*> slide <*> (getPacing <$> newData) <*> inpt)
        en = (getPacing <$> newData) .||. slide
        dflt = repeat 0 :: Vec 4 Int

        nextWindow :: Vec 4 Int 
            -> Bool 
            -> Bool 
            -> Int 
            -> Vec 4 Int
        nextWindow win toSlide newData dta = out
            where
                out = case (toSlide, newData) of
                    (False, False) -> win
                    (False, True) -> lastBucketUpdated
                    (True, False) -> 0 +>> win
                    (True, True) -> 0 +>> lastBucketUpdated
                lastBucketUpdated = 
                    replace 0 (windowFunc1 (head win) dta) win

slidingWindow1 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut1 
    -> Signal dom Bool 
    -> Signal dom Tag 
    -> Signal dom Bool 
    -> Signal dom (Tag, (Vec 6 Int)) 
slidingWindow1 newData slide tag inpt = window
    where
        window = register (invalidTag, dflt) (mux en next window)
        next = bundle (tag, nextWindow <$> (snd <$> window) 
                <*> slide <*> (getPacing <$> newData) <*> inpt)
        en = (getPacing <$> newData) .||. slide
        dflt = repeat 0 :: Vec 6 Int

        nextWindow :: Vec 6 Int 
            -> Bool 
            -> Bool 
            -> Bool 
            -> Vec 6 Int
        nextWindow win toSlide newData dta = out
            where
                out = case (toSlide, newData) of
                    (False, False) -> win
                    (False, True) -> lastBucketUpdated
                    (True, False) -> 0 +>> win
                    (True, True) -> 0 +>> lastBucketUpdated
                lastBucketUpdated = 
                    replace 0 (windowFunc0 (head win) dta) win




---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom 
    => Signal dom Inputs 
    -> Signal dom Outputs
monitor inputs = outputs
    where 
        (newEvent, event) = unbundle (hlc inputs)

        (qPushValid, qPopValid, qPopData) = 
            unbundle (queue (bundle (qPush, qPop, qInptData)))
        qPush = newEvent
        qPop = toPop
        qInptData = event

        (toPop, outputs) = unbundle (llc (bundle (qPopValid, qPopData)))


---------------------------------------------------------------

topEntity :: Clock TestDomain 
    -> Reset TestDomain 
    -> Enable TestDomain 
    -> Signal TestDomain Inputs 
    -> Signal TestDomain Outputs
topEntity clk rst en inputs = 
    exposeClockResetEnable (monitor inputs) clk rst en
