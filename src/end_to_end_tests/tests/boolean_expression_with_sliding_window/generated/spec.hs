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
-- output e @2kHz := d.aggregate(over: 0.01s, using: count)
-- output f @1kHz :=  x.hold(or: 0) < 0
-- 

---------------------------------------------------------------

-- Evaluation Order
--------------------
-- x
-- a
-- b, c
-- d
-- sw(d,e)
-- e, f

-- Memory Window
-----------------
-- window e = 1
-- window d = 3
-- window a = 5
-- window b = 4
-- window x = 5
-- window f = 1
-- window c = 4
-- window sw(d,e) = 1

-- Pipeline Visualization
--------------------------

-- x       | x       | x       | x       | x       | x       | x       | x       | x       | x      
-- -------------------------------------------------------------------------------------------------
--         | a       | a       | a       | a       | a       | a       | a       | a       | a      
-- -------------------------------------------------------------------------------------------------
--         |         | b,c     | b,c     | b,c     | b,c     | b,c     | b,c     | b,c     | b,c    
-- -------------------------------------------------------------------------------------------------
--         |         |         | d       | d       | d       | d       | d       | d       | d      
-- -------------------------------------------------------------------------------------------------
--         |         |         |         | sw(d,e) | sw(d,e) | sw(d,e) | sw(d,e) | sw(d,e) | sw(d,e)
-- -------------------------------------------------------------------------------------------------
--         |         |         |         |         | e,f     | e,f     | e,f     | e,f     | e,f    
-- -------------------------------------------------------------------------------------------------

-- Nicknames
-------------
-- input0 = x
-- output0 = a
-- output1 = b
-- output2 = c
-- output3 = d
-- output4 = e
-- output5 = f
-- sw0 = sw(d,e)

---------------------------------------------------------------

data ValidInt = ValidInt {
    value :: Int,
    valid :: Bool
} deriving (Generic, NFDataX)

data ValidBool = ValidBool {
    value :: Bool,
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
    output3 :: ValidBool,
    output4 :: ValidInt,
    output5 :: ValidBool
} deriving (Generic, NFDataX)


class Pacing a where getPacing :: a -> Bool

data PacingIn0 = PacingIn0 Bool deriving (Generic, NFDataX)
data PacingOut0 = PacingOut0 Bool deriving (Generic, NFDataX)
data PacingOut1 = PacingOut1 Bool deriving (Generic, NFDataX)
data PacingOut2 = PacingOut2 Bool deriving (Generic, NFDataX)
data PacingOut3 = PacingOut3 Bool deriving (Generic, NFDataX)
data PacingOut4 = PacingOut4 Bool deriving (Generic, NFDataX)
data PacingOut5 = PacingOut5 Bool deriving (Generic, NFDataX)

instance Pacing PacingIn0 where getPacing (PacingIn0 x) = x
instance Pacing PacingOut0 where getPacing (PacingOut0 x) = x
instance Pacing PacingOut1 where getPacing (PacingOut1 x) = x
instance Pacing PacingOut2 where getPacing (PacingOut2 x) = x
instance Pacing PacingOut3 where getPacing (PacingOut3 x) = x
instance Pacing PacingOut4 where getPacing (PacingOut4 x) = x
instance Pacing PacingOut5 where getPacing (PacingOut5 x) = x

data Pacings = Pacings {
    pacingIn0 :: PacingIn0,
    pacingOut0 :: PacingOut0,
    pacingOut1 :: PacingOut1,
    pacingOut2 :: PacingOut2,
    pacingOut3 :: PacingOut3,
    pacingOut4 :: PacingOut4,
    pacingOut5 :: PacingOut5
} deriving (Generic, NFDataX)

data DebugEnables = DebugEnables {
    enableIn0 :: Bool,
    enableOut0 :: Bool,
    enableOut1 :: Bool,
    enableOut2 :: Bool,
    enableOut3 :: Bool,
    enableOut4 :: Bool,
    enableOut5 :: Bool
} deriving (Generic, NFDataX)

-- using newtype to avoid flattening of data
-- https://clash-lang.discourse.group/t/how-to-avoid-flattening-of-fields-in-record/79/5
newtype Slides = Slides {
    slide0 :: Bool
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
    slide0 :: Tag
} deriving (Generic, NFDataX)

type Event = (Inputs, Slides, Pacings)

nullEvent :: Event
nullEvent = (nullInputs, nullSlides, nullPacings)
nullInputs = Inputs (ValidInt 0 False) 
nullSlides = Slides False 
nullPacings = Pacings 
                nullPacingIn0
                nullPacingOut0
                nullPacingOut1
                nullPacingOut2
                nullPacingOut3
                nullPacingOut4
                nullPacingOut5
nullPacingIn0 = PacingIn0 False
nullPacingOut0 = PacingOut0 False 
nullPacingOut1 = PacingOut1 False 
nullPacingOut2 = PacingOut2 False 
nullPacingOut3 = PacingOut3 False 
nullPacingOut4 = PacingOut4 False 
nullPacingOut5 = PacingOut5 False 


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
        newEvent = hasInput0 .||. timer0Over .||. timer1Over

        event = bundle (inputs, slides, pacings)

        slides = Slides <$> s0
        pacings = Pacings <$> pIn0 
                    <*> pOut0 
                    <*> pOut1 
                    <*> pOut2 
                    <*> pOut3 
                    <*> pOut4 
                    <*> pOut5

        hasInput0 = ((.valid). (.input0)) <$> inputs

        pIn0 = PacingIn0 <$> hasInput0
        pOut0 = PacingOut0 <$> timer0Over
        pOut1 = PacingOut1 <$> timer0Over
        pOut2 = PacingOut2 <$> timer0Over
        pOut3 = PacingOut3 <$> timer0Over
        pOut4 = PacingOut4 <$> timer0Over
        pOut5 = PacingOut5 <$> timer1Over

        s0 = timer0Over

        timer0Over = timer0 .>=. period0InNs
        timer0 = timer timer0Over
        period0InNs = 500000
        timer1Over = timer1 .>=. period1InNs
        timer1 = timer timer1Over
        period1InNs = 1000000

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
maxTag = 22 :: Tag
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
    -> Signal dom ((Bool, Outputs), (Slides, DebugEnables))
llc event = bundle (bundle (toPop, outputs), debugSignals)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        toPop = pure True

        (inputs, slides, pacings) = unbundle poppedEvent

        input0 = (.input0) <$> inputs

        slide0 = (.slide0) <$> slides

        pIn0 = (.pacingIn0) <$> pacings
        pOut0 = (.pacingOut0) <$> pacings
        pOut1 = (.pacingOut1) <$> pacings
        pOut2 = (.pacingOut2) <$> pacings
        pOut3 = (.pacingOut3) <$> pacings
        pOut4 = (.pacingOut4) <$> pacings
        pOut5 = (.pacingOut5) <$> pacings
        
        tIn0 = genTag (getPacing <$> pIn0)
        tOut0 = genTag (getPacing <$> pOut0)
        tOut1 = genTag (getPacing <$> pOut1)
        tOut2 = genTag (getPacing <$> pOut2)
        tOut3 = genTag (getPacing <$> pOut3)
        tSw0 = genTag (getPacing <$> pOut3)
        tOut4 = genTag (getPacing <$> pOut4)
        tOut5 = genTag (getPacing <$> pOut5)

        -- tag generation takes 1 cycle so we need to delay the input
        input0Data = delay 0 (((.value). (.input0)) <$> inputs)

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
                <*> tOut0 
                <*> tOut1 
                <*> tOut2 
                <*> tOut3 
                <*> tOut4 
                <*> tOut5 
                <*> tSw0
        curTagsLevel0 = curTags
        curTagsLevel1 = delayFor d1 tagsDefault curTags
        curTagsLevel2 = delayFor d2 tagsDefault curTags
        curTagsLevel3 = delayFor d3 tagsDefault curTags
        curTagsLevel4 = delayFor d4 tagsDefault curTags
        curTagsLevel5 = delayFor d5 tagsDefault curTags
        curTagsLevel6 = delayFor d6 tagsDefault curTags
        nullT = invalidTag

        enIn0 = delayFor d1 nullPacingIn0 pIn0
        enOut0 = delayFor d2 nullPacingOut0 pOut0
        enOut1 = delayFor d3 nullPacingOut1 pOut1
        enOut2 = delayFor d3 nullPacingOut2 pOut2
        enOut3 = delayFor d4 nullPacingOut3 pOut3
        enSw0 = delayFor d5 nullPacingOut3 pOut3
        sld0 = delayFor d5 False slide0
        enOut4 = delayFor d6 nullPacingOut4 pOut4
        enOut5 = delayFor d6 nullPacingOut5 pOut5

        output0Aktv = delayFor d7 False (getPacing <$> pOut0)
        output1Aktv = delayFor d7 False (getPacing <$> pOut1)
        output2Aktv = delayFor d7 False (getPacing <$> pOut2)
        output3Aktv = delayFor d7 False (getPacing <$> pOut3)
        output4Aktv = delayFor d7 False (getPacing <$> pOut4)
        output5Aktv = delayFor d7 False (getPacing <$> pOut5)

        -- Evaluation of input windows: level 0
        input0Win = input0Window enIn0 tIn0 input0Data

        -- Evaluation of output 0: level 1
        out0 = outputStream0 enOut0 
            ((.output0) <$> curTagsLevel1) 
            out0Data0 
        out0Data0 = getLatestValue 
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

        -- Evaluation of output 4: level 5
        out4 = outputStream4 enOut4 
            ((.output4) <$> curTagsLevel5) 
            out4Data0 
        (_, out4Data0) = unbundle sw0

        -- Evaluation of output 5: level 5
        out5 = outputStream5 enOut5 
            ((.output5) <$> curTagsLevel5) 
            out5Data0 
        out5Data0 = getLatestValue 
            <$> input0Win 
            <*> out5Data0Dflt
        out5Data0Dflt = pure (0)

        -- Evaluation of sliding window 0: level 4
        sw0 = slidingWindow0 enSw0 sld0 
            ((.slide0) <$> curTagsLevel4) sw0Data
        sw0Data = getMatchingTag 
            <$> out3 
            <*> ((.output3) <$> curTagsLevel4) 
            <*> (pure False)

        -- Outputing all results: level 6
        output0 = ValidInt <$> output0Data <*> output0Aktv
        output0Data = getMatchingTag 
            <$> out0 
            <*> ((.output0) 
            <$> curTagsLevel6) 
            <*> (pure 0)
        output1 = ValidBool <$> output1Data <*> output1Aktv
        output1Data = getMatchingTag 
            <$> out1 
            <*> ((.output1) 
            <$> curTagsLevel6) 
            <*> (pure False)
        output2 = ValidBool <$> output2Data <*> output2Aktv
        output2Data = getMatchingTag 
            <$> out2 
            <*> ((.output2) 
            <$> curTagsLevel6) 
            <*> (pure False)
        output3 = ValidBool <$> output3Data <*> output3Aktv
        output3Data = getMatchingTag 
            <$> out3 
            <*> ((.output3) 
            <$> curTagsLevel6) 
            <*> (pure False)
        output4 = ValidInt <$> output4Data <*> output4Aktv
        (_, output4Data) = unbundle out4
        output5 = ValidBool <$> output5Data <*> output5Aktv
        (_, output5Data) = unbundle out5

        outputs = Outputs 
            <$> output0 
            <*> output1 
            <*> output2 
            <*> output3 
            <*> output4 
            <*> output5

        debugSignals = bundle (slides, debugEnables)
        debugEnables = DebugEnables <$>
                            (getPacing <$> enIn0) <*>
                            (getPacing <$> enOut0) <*>
                            (getPacing <$> enOut1) <*>
                            (getPacing <$> enOut2) <*>
                            (getPacing <$> enOut3) <*>
                            (getPacing <$> enOut4) <*>
                            (getPacing <$> enOut5)

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
    -> Signal dom (Vec 5 (Tag, Int))
input0Window en tag val = result
    where result = register (repeat (invalidTag, 0)) 
                    (mux (getPacing <$> en) 
                        ((<<+) <$> result <*> (bundle (tag, val)))
                        result)



outputStream0 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut0 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Vec 5 (Tag, Int))
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
    -> Signal dom (Vec 4 (Tag, Bool))
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
    -> Signal dom (Vec 4 (Tag, Bool))
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
    -> Signal dom (Vec 3 (Tag, Bool))
outputStream3 en tag out1_00 out2_01 out1_10 out2_11 = result
    where
        result = register (repeat (invalidTag, False)) 
                (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = ((out1_00 .&&. out2_01) .||. (out1_10 .&&. out2_11))


outputStream4 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut4 
    -> Signal dom Tag 
    -> Signal dom (Vec 21 Int) 
    -> Signal dom (Tag, Int)
outputStream4 en tag sw0 = result
    where
        result = register (invalidTag, 0) 
                (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = (merge0 <$> sw0)
        merge0 :: Vec 21 Int -> Int
        merge0 win = fold windowFunc1 (tail win)


outputStream5 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut5 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Tag, Bool)
outputStream5 en tag in0_0 = result
    where
        result = register (invalidTag, False) 
                (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = (in0_0 .<. (0))



windowFunc0 :: Int -> Bool -> Int
windowFunc0 acc item = acc + 1

windowFunc1 :: Int -> Int -> Int
windowFunc1 acc item = acc + item


slidingWindow0 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut3 
    -> Signal dom Bool 
    -> Signal dom Tag 
    -> Signal dom Bool 
    -> Signal dom (Tag, (Vec 21 Int)) 
slidingWindow0 newData slide tag inpt = window
    where
        window = register (invalidTag, dflt) (mux en next window)
        next = bundle (tag, nextWindow <$> (snd <$> window) 
                <*> slide <*> (getPacing <$> newData) <*> inpt)
        en = (getPacing <$> newData) .||. slide
        dflt = repeat 0 :: Vec 21 Int

        nextWindow :: Vec 21 Int 
            -> Bool 
            -> Bool 
            -> Bool 
            -> Vec 21 Int
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
    -> Signal dom (Outputs, (QPush, QPop, QPushValid, QPopValid, Slides, DebugEnables))
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

        (llcSlides, debugEnables) = unbundle llcDebug
        debugSignals = bundle (qPush, qPop, qPushValid, qPopValid, llcSlides, debugEnables)


---------------------------------------------------------------

topEntity :: Clock TestDomain 
    -> Reset TestDomain 
    -> Enable TestDomain 
    -> Signal TestDomain Inputs 
    -> Signal TestDomain (Outputs, (QPush, QPop, QPushValid, QPopValid, Slides, DebugEnables))
topEntity clk rst en inputs = 
    exposeClockResetEnable (monitor inputs) clk rst en
