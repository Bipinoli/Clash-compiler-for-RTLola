{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Spec where

import Clash.Prelude

---------------------------------------------------------------

-- input acceleration_x: Int64
-- input gps_sats: UInt64
-- input lat_gps: Int64
-- 
-- output acceleration_x_periodic @2kHz := acceleration_x.hold(or: 0) 
-- output acceleration_x_rising := delta(acceleration_x_periodic, dft: acceleration_x_periodic) > 5
-- output acceleration_x_sinking := delta(acceleration_x_periodic, dft: acceleration_x_periodic) < -5 
-- 
-- output acceleration_x_direction_change := (acceleration_x_rising ∧ acceleration_x_sinking.offset(by: -1).defaults(to: false)) ∨ (acceleration_x_sinking ∧ acceleration_x_rising.offset(by: -1).defaults(to: false))
-- 
-- output acceleration_x_changes @2kHz := acceleration_x_direction_change.aggregate(over: 0.05s, using: count)
-- output trigger_acc := acceleration_x_changes > 5
-- 
-- output gps_missed_beat: Bool @ 2kHz := lat_gps.aggregate(over: 0.055s, using: count) < 1
-- output gps_medium_loss: Bool @ 0.1kHz := lat_gps.aggregate(over: 0.01s, using: count) < 15 && lat_gps.aggregate(over: 0.01s, using: count) >= 10
-- output gps_high_loss: Bool @ 0.1kHz := lat_gps.aggregate(over: 0.01s, using: count) < 10 && lat_gps.aggregate(over: 0.01s, using: count) >= 5
-- output gps_very_high_loss: Bool @ 0.1kHz := lat_gps.aggregate(over: 0.01s, using: count) < 5
-- output trigger_gps_sats: Bool @ 0.1kHz := gps_sats.hold(or: 0)  < 6
-- 

---------------------------------------------------------------

-- Evaluation Order
--------------------
-- gps_sats, acceleration_x, lat_gps
-- acceleration_x_periodic
-- acceleration_x_rising, acceleration_x_sinking
-- acceleration_x_direction_change
-- sw(acceleration_x_direction_change,acceleration_x_changes)
-- acceleration_x_changes, sw(lat_gps,gps_high_loss), sw(lat_gps,gps_high_loss), sw(lat_gps,gps_medium_loss), sw(lat_gps,gps_medium_loss), sw(lat_gps,gps_missed_beat), sw(lat_gps,gps_very_high_loss)
-- trigger_acc, gps_high_loss, gps_medium_loss, gps_missed_beat, gps_very_high_loss, trigger_gps_sats

-- Memory Window
-----------------
-- window sw(acceleration_x_direction_change,acceleration_x_changes) = 1
-- window sw(lat_gps,gps_medium_loss) = 1
-- window sw(lat_gps,gps_missed_beat) = 1
-- window lat_gps = 5
-- window acceleration_x = 1
-- window acceleration_x_direction_change = 4
-- window acceleration_x_periodic = 6
-- window sw(lat_gps,gps_high_loss) = 1
-- window gps_medium_loss = 1
-- window gps_sats = 6
-- window acceleration_x_rising = 5
-- window sw(lat_gps,gps_high_loss) = 1
-- window sw(lat_gps,gps_medium_loss) = 1
-- window sw(lat_gps,gps_very_high_loss) = 1
-- window trigger_gps_sats = 1
-- window acceleration_x_changes = 2
-- window trigger_acc = 1
-- window acceleration_x_sinking = 5
-- window gps_missed_beat = 1
-- window gps_high_loss = 1
-- window gps_very_high_loss = 1

-- Pipeline Visualization
--------------------------

-- gps_sats,acceleration_x,lat_gps                                                                                                                                                               | gps_sats,acceleration_x,lat_gps                                                                                                                                                               | gps_sats,acceleration_x,lat_gps                                                                                                                                                               | gps_sats,acceleration_x,lat_gps                                                                                                                                                               | gps_sats,acceleration_x,lat_gps                                                                                                                                                               | gps_sats,acceleration_x,lat_gps                                                                                                                                                               | gps_sats,acceleration_x,lat_gps                                                                                                                                                               | gps_sats,acceleration_x,lat_gps                                                                                                                                                               | gps_sats,acceleration_x,lat_gps                                                                                                                                                               | gps_sats,acceleration_x,lat_gps                                                                                                                                                              
-- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--                                                                                                                                                                                               | acceleration_x_periodic                                                                                                                                                                       | acceleration_x_periodic                                                                                                                                                                       | acceleration_x_periodic                                                                                                                                                                       | acceleration_x_periodic                                                                                                                                                                       | acceleration_x_periodic                                                                                                                                                                       | acceleration_x_periodic                                                                                                                                                                       | acceleration_x_periodic                                                                                                                                                                       | acceleration_x_periodic                                                                                                                                                                       | acceleration_x_periodic                                                                                                                                                                      
-- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--                                                                                                                                                                                               |                                                                                                                                                                                               | acceleration_x_rising,acceleration_x_sinking                                                                                                                                                  | acceleration_x_rising,acceleration_x_sinking                                                                                                                                                  | acceleration_x_rising,acceleration_x_sinking                                                                                                                                                  | acceleration_x_rising,acceleration_x_sinking                                                                                                                                                  | acceleration_x_rising,acceleration_x_sinking                                                                                                                                                  | acceleration_x_rising,acceleration_x_sinking                                                                                                                                                  | acceleration_x_rising,acceleration_x_sinking                                                                                                                                                  | acceleration_x_rising,acceleration_x_sinking                                                                                                                                                 
-- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--                                                                                                                                                                                               |                                                                                                                                                                                               |                                                                                                                                                                                               | acceleration_x_direction_change                                                                                                                                                               | acceleration_x_direction_change                                                                                                                                                               | acceleration_x_direction_change                                                                                                                                                               | acceleration_x_direction_change                                                                                                                                                               | acceleration_x_direction_change                                                                                                                                                               | acceleration_x_direction_change                                                                                                                                                               | acceleration_x_direction_change                                                                                                                                                              
-- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--                                                                                                                                                                                               |                                                                                                                                                                                               |                                                                                                                                                                                               |                                                                                                                                                                                               | sw(acceleration_x_direction_change,acceleration_x_changes)                                                                                                                                    | sw(acceleration_x_direction_change,acceleration_x_changes)                                                                                                                                    | sw(acceleration_x_direction_change,acceleration_x_changes)                                                                                                                                    | sw(acceleration_x_direction_change,acceleration_x_changes)                                                                                                                                    | sw(acceleration_x_direction_change,acceleration_x_changes)                                                                                                                                    | sw(acceleration_x_direction_change,acceleration_x_changes)                                                                                                                                   
-- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--                                                                                                                                                                                               |                                                                                                                                                                                               |                                                                                                                                                                                               |                                                                                                                                                                                               |                                                                                                                                                                                               | acceleration_x_changes,sw(lat_gps,gps_high_loss),sw(lat_gps,gps_high_loss),sw(lat_gps,gps_medium_loss),sw(lat_gps,gps_medium_loss),sw(lat_gps,gps_missed_beat),sw(lat_gps,gps_very_high_loss) | acceleration_x_changes,sw(lat_gps,gps_high_loss),sw(lat_gps,gps_high_loss),sw(lat_gps,gps_medium_loss),sw(lat_gps,gps_medium_loss),sw(lat_gps,gps_missed_beat),sw(lat_gps,gps_very_high_loss) | acceleration_x_changes,sw(lat_gps,gps_high_loss),sw(lat_gps,gps_high_loss),sw(lat_gps,gps_medium_loss),sw(lat_gps,gps_medium_loss),sw(lat_gps,gps_missed_beat),sw(lat_gps,gps_very_high_loss) | acceleration_x_changes,sw(lat_gps,gps_high_loss),sw(lat_gps,gps_high_loss),sw(lat_gps,gps_medium_loss),sw(lat_gps,gps_medium_loss),sw(lat_gps,gps_missed_beat),sw(lat_gps,gps_very_high_loss) | acceleration_x_changes,sw(lat_gps,gps_high_loss),sw(lat_gps,gps_high_loss),sw(lat_gps,gps_medium_loss),sw(lat_gps,gps_medium_loss),sw(lat_gps,gps_missed_beat),sw(lat_gps,gps_very_high_loss)
-- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--                                                                                                                                                                                               |                                                                                                                                                                                               |                                                                                                                                                                                               |                                                                                                                                                                                               |                                                                                                                                                                                               |                                                                                                                                                                                               | trigger_acc,gps_high_loss,gps_medium_loss,gps_missed_beat,gps_very_high_loss,trigger_gps_sats                                                                                                 | trigger_acc,gps_high_loss,gps_medium_loss,gps_missed_beat,gps_very_high_loss,trigger_gps_sats                                                                                                 | trigger_acc,gps_high_loss,gps_medium_loss,gps_missed_beat,gps_very_high_loss,trigger_gps_sats                                                                                                 | trigger_acc,gps_high_loss,gps_medium_loss,gps_missed_beat,gps_very_high_loss,trigger_gps_sats                                                                                                
-- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Nicknames
-------------
-- input0 = acceleration_x
-- input1 = gps_sats
-- input2 = lat_gps
-- output0 = acceleration_x_periodic
-- output1 = acceleration_x_rising
-- output2 = acceleration_x_sinking
-- output3 = acceleration_x_direction_change
-- output4 = acceleration_x_changes
-- output5 = trigger_acc
-- output6 = gps_missed_beat
-- output7 = gps_medium_loss
-- output8 = gps_high_loss
-- output9 = gps_very_high_loss
-- output10 = trigger_gps_sats
-- sw0 = sw(acceleration_x_direction_change,acceleration_x_changes)
-- sw1 = sw(lat_gps,gps_missed_beat)
-- sw2 = sw(lat_gps,gps_medium_loss)
-- sw3 = sw(lat_gps,gps_medium_loss)
-- sw4 = sw(lat_gps,gps_high_loss)
-- sw5 = sw(lat_gps,gps_high_loss)
-- sw6 = sw(lat_gps,gps_very_high_loss)

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
    output0 :: ValidInt,
    output1 :: ValidBool,
    output2 :: ValidBool,
    output3 :: ValidBool,
    output4 :: ValidInt,
    output5 :: ValidBool,
    output6 :: ValidBool,
    output7 :: ValidBool,
    output8 :: ValidBool,
    output9 :: ValidBool,
    output10 :: ValidBool
} deriving (Generic, NFDataX)


class Pacing a where getPacing :: a -> Bool

data PacingIn0 = PacingIn0 Bool deriving (Generic, NFDataX)
data PacingIn1 = PacingIn1 Bool deriving (Generic, NFDataX)
data PacingIn2 = PacingIn2 Bool deriving (Generic, NFDataX)
data PacingOut0 = PacingOut0 Bool deriving (Generic, NFDataX)
data PacingOut1 = PacingOut1 Bool deriving (Generic, NFDataX)
data PacingOut2 = PacingOut2 Bool deriving (Generic, NFDataX)
data PacingOut3 = PacingOut3 Bool deriving (Generic, NFDataX)
data PacingOut4 = PacingOut4 Bool deriving (Generic, NFDataX)
data PacingOut5 = PacingOut5 Bool deriving (Generic, NFDataX)
data PacingOut6 = PacingOut6 Bool deriving (Generic, NFDataX)
data PacingOut7 = PacingOut7 Bool deriving (Generic, NFDataX)
data PacingOut8 = PacingOut8 Bool deriving (Generic, NFDataX)
data PacingOut9 = PacingOut9 Bool deriving (Generic, NFDataX)
data PacingOut10 = PacingOut10 Bool deriving (Generic, NFDataX)

instance Pacing PacingIn0 where getPacing (PacingIn0 x) = x
instance Pacing PacingIn1 where getPacing (PacingIn1 x) = x
instance Pacing PacingIn2 where getPacing (PacingIn2 x) = x
instance Pacing PacingOut0 where getPacing (PacingOut0 x) = x
instance Pacing PacingOut1 where getPacing (PacingOut1 x) = x
instance Pacing PacingOut2 where getPacing (PacingOut2 x) = x
instance Pacing PacingOut3 where getPacing (PacingOut3 x) = x
instance Pacing PacingOut4 where getPacing (PacingOut4 x) = x
instance Pacing PacingOut5 where getPacing (PacingOut5 x) = x
instance Pacing PacingOut6 where getPacing (PacingOut6 x) = x
instance Pacing PacingOut7 where getPacing (PacingOut7 x) = x
instance Pacing PacingOut8 where getPacing (PacingOut8 x) = x
instance Pacing PacingOut9 where getPacing (PacingOut9 x) = x
instance Pacing PacingOut10 where getPacing (PacingOut10 x) = x

data Pacings = Pacings {
    pacingIn0 :: PacingIn0,
    pacingIn1 :: PacingIn1,
    pacingIn2 :: PacingIn2,
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
    pacingOut10 :: PacingOut10
} deriving (Generic, NFDataX)

data DebugEnables = DebugEnables {
    enableIn0 :: Bool,
    enableIn1 :: Bool,
    enableIn2 :: Bool,
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
    enableOut10 :: Bool
} deriving (Generic, NFDataX)

data Slides = Slides {
    slide0 :: Bool,
    slide1 :: Bool,
    slide2 :: Bool,
    slide3 :: Bool,
    slide4 :: Bool,
    slide5 :: Bool,
    slide6 :: Bool
} deriving (Generic, NFDataX)

type Tag = Unsigned 8

data Tags = Tags {
    input0 :: Tag,
    input1 :: Tag,
    input2 :: Tag,
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
    slide0 :: Tag,
    slide1 :: Tag,
    slide2 :: Tag,
    slide3 :: Tag,
    slide4 :: Tag,
    slide5 :: Tag,
    slide6 :: Tag
} deriving (Generic, NFDataX)

type Event = (Inputs, Slides, Pacings)

nullEvent :: Event
nullEvent = (nullInputs, nullSlides, nullPacings)
nullInputs = Inputs (ValidInt 0 False) (ValidInt 0 False) (ValidInt 0 False) 
nullSlides = Slides False False False False False False False 
nullPacings = Pacings 
                nullPacingIn0
                nullPacingIn1
                nullPacingIn2
                nullPacingOut0
                nullPacingOut1
                nullPacingOut2
                nullPacingOut3
                nullPacingOut4
                nullPacingOut5
                nullPacingOut6
                nullPacingOut7
                nullPacingOut8
                nullPacingOut9
                nullPacingOut10
nullPacingIn0 = PacingIn0 False
nullPacingIn1 = PacingIn1 False
nullPacingIn2 = PacingIn2 False
nullPacingOut0 = PacingOut0 False 
nullPacingOut1 = PacingOut1 False 
nullPacingOut2 = PacingOut2 False 
nullPacingOut3 = PacingOut3 False 
nullPacingOut4 = PacingOut4 False 
nullPacingOut5 = PacingOut5 False 
nullPacingOut6 = PacingOut6 False 
nullPacingOut7 = PacingOut7 False 
nullPacingOut8 = PacingOut8 False 
nullPacingOut9 = PacingOut9 False 
nullPacingOut10 = PacingOut10 False 


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
        newEvent = hasInput0 .||. hasInput1 .||. hasInput2 .||. timer0Over .||. timer1Over

        event = bundle (inputs, slides, pacings)

        slides = Slides <$> s0 
                    <*> s1 
                    <*> s2 
                    <*> s3 
                    <*> s4 
                    <*> s5 
                    <*> s6
        pacings = Pacings <$> pIn0 
                    <*> pIn1 
                    <*> pIn2 
                    <*> pOut0 
                    <*> pOut1 
                    <*> pOut2 
                    <*> pOut3 
                    <*> pOut4 
                    <*> pOut5 
                    <*> pOut6 
                    <*> pOut7 
                    <*> pOut8 
                    <*> pOut9 
                    <*> pOut10

        hasInput0 = ((.valid). (.input0)) <$> inputs
        hasInput1 = ((.valid). (.input1)) <$> inputs
        hasInput2 = ((.valid). (.input2)) <$> inputs

        pIn0 = PacingIn0 <$> hasInput0
        pIn1 = PacingIn1 <$> hasInput1
        pIn2 = PacingIn2 <$> hasInput2
        pOut0 = PacingOut0 <$> timer0Over
        pOut1 = PacingOut1 <$> timer0Over
        pOut2 = PacingOut2 <$> timer0Over
        pOut3 = PacingOut3 <$> timer0Over
        pOut4 = PacingOut4 <$> timer0Over
        pOut5 = PacingOut5 <$> timer0Over
        pOut6 = PacingOut6 <$> timer0Over
        pOut7 = PacingOut7 <$> timer1Over
        pOut8 = PacingOut8 <$> timer1Over
        pOut9 = PacingOut9 <$> timer1Over
        pOut10 = PacingOut10 <$> timer1Over

        s0 = timer0Over
        s1 = timer0Over
        s2 = timer1Over
        s3 = timer1Over
        s4 = timer1Over
        s5 = timer1Over
        s6 = timer1Over

        timer0Over = timer0 .>=. period0InNs
        timer0 = timer timer0Over
        period0InNs = 500000
        timer1Over = timer1 .>=. period1InNs
        timer1 = timer timer1Over
        period1InNs = 10000000

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
maxTag = 112 :: Tag
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
        input1 = (.input1) <$> inputs
        input2 = (.input2) <$> inputs

        slide0 = (.slide0) <$> slides
        slide1 = (.slide1) <$> slides
        slide2 = (.slide2) <$> slides
        slide3 = (.slide3) <$> slides
        slide4 = (.slide4) <$> slides
        slide5 = (.slide5) <$> slides
        slide6 = (.slide6) <$> slides

        pIn0 = (.pacingIn0) <$> pacings
        pIn1 = (.pacingIn1) <$> pacings
        pIn2 = (.pacingIn2) <$> pacings
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
        
        tIn1 = genTag (getPacing <$> pIn1)
        tIn0 = genTag (getPacing <$> pIn0)
        tIn2 = genTag (getPacing <$> pIn2)
        tOut0 = genTag (getPacing <$> pOut0)
        tOut1 = genTag (getPacing <$> pOut1)
        tOut2 = genTag (getPacing <$> pOut2)
        tOut3 = genTag (getPacing <$> pOut3)
        tSw0 = genTag (getPacing <$> pOut3)
        tOut4 = genTag (getPacing <$> pOut4)
        tSw5 = genTag (getPacing <$> pIn2)
        tSw4 = genTag (getPacing <$> pIn2)
        tSw2 = genTag (getPacing <$> pIn2)
        tSw3 = genTag (getPacing <$> pIn2)
        tSw1 = genTag (getPacing <$> pIn2)
        tSw6 = genTag (getPacing <$> pIn2)
        tOut5 = genTag (getPacing <$> pOut5)
        tOut8 = genTag (getPacing <$> pOut8)
        tOut7 = genTag (getPacing <$> pOut7)
        tOut6 = genTag (getPacing <$> pOut6)
        tOut9 = genTag (getPacing <$> pOut9)
        tOut10 = genTag (getPacing <$> pOut10)

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
                nullT 
                nullT 
                nullT 
                nullT 
                nullT 
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
                <*> tOut3 
                <*> tOut4 
                <*> tOut5 
                <*> tOut6 
                <*> tOut7 
                <*> tOut8 
                <*> tOut9 
                <*> tOut10 
                <*> tSw0 
                <*> tSw1 
                <*> tSw2 
                <*> tSw3 
                <*> tSw4 
                <*> tSw5 
                <*> tSw6
        curTagsLevel1 = delayFor d1 tagsDefault curTags
        curTagsLevel2 = delayFor d2 tagsDefault curTags
        curTagsLevel3 = delayFor d3 tagsDefault curTags
        curTagsLevel4 = delayFor d4 tagsDefault curTags
        curTagsLevel5 = delayFor d5 tagsDefault curTags
        curTagsLevel6 = delayFor d6 tagsDefault curTags
        curTagsLevel7 = delayFor d7 tagsDefault curTags
        nullT = invalidTag

        enIn1 = delayFor d1 nullPacingIn1 pIn1
        enIn0 = delayFor d1 nullPacingIn0 pIn0
        enIn2 = delayFor d1 nullPacingIn2 pIn2
        enOut0 = delayFor d2 nullPacingOut0 pOut0
        enOut1 = delayFor d3 nullPacingOut1 pOut1
        enOut2 = delayFor d3 nullPacingOut2 pOut2
        enOut3 = delayFor d4 nullPacingOut3 pOut3
        enSw0 = delayFor d5 nullPacingOut3 pOut3
        sld0 = delayFor d5 False slide0
        enOut4 = delayFor d6 nullPacingOut4 pOut4
        enSw5 = delayFor d6 nullPacingIn2 pIn2
        sld5 = delayFor d6 False slide5
        enSw4 = delayFor d6 nullPacingIn2 pIn2
        sld4 = delayFor d6 False slide4
        enSw2 = delayFor d6 nullPacingIn2 pIn2
        sld2 = delayFor d6 False slide2
        enSw3 = delayFor d6 nullPacingIn2 pIn2
        sld3 = delayFor d6 False slide3
        enSw1 = delayFor d6 nullPacingIn2 pIn2
        sld1 = delayFor d6 False slide1
        enSw6 = delayFor d6 nullPacingIn2 pIn2
        sld6 = delayFor d6 False slide6
        enOut5 = delayFor d7 nullPacingOut5 pOut5
        enOut8 = delayFor d7 nullPacingOut8 pOut8
        enOut7 = delayFor d7 nullPacingOut7 pOut7
        enOut6 = delayFor d7 nullPacingOut6 pOut6
        enOut9 = delayFor d7 nullPacingOut9 pOut9
        enOut10 = delayFor d7 nullPacingOut10 pOut10

        output0Aktv = delayFor d8 False (getPacing <$> pOut0)
        output1Aktv = delayFor d8 False (getPacing <$> pOut1)
        output2Aktv = delayFor d8 False (getPacing <$> pOut2)
        output3Aktv = delayFor d8 False (getPacing <$> pOut3)
        output4Aktv = delayFor d8 False (getPacing <$> pOut4)
        output5Aktv = delayFor d8 False (getPacing <$> pOut5)
        output6Aktv = delayFor d8 False (getPacing <$> pOut6)
        output7Aktv = delayFor d8 False (getPacing <$> pOut7)
        output8Aktv = delayFor d8 False (getPacing <$> pOut8)
        output9Aktv = delayFor d8 False (getPacing <$> pOut9)
        output10Aktv = delayFor d8 False (getPacing <$> pOut10)

        -- Evaluation of input windows: level 0
        input0Win = input0Window enIn0 tIn0 input0Data
        input1Win = input1Window enIn1 tIn1 input1Data
        input2Win = input2Window enIn2 tIn2 input2Data

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
        out3Data2 = getMatchingTag 
            <$> out2 
            <*> ((.output2) <$> curTagsLevel3) 
            <*> (pure (False))
        out3Data3 = getOffset         
            <$> out1 
            <*> ((.output1) <$> curTagsLevel3) 
            <*> (pure 1) 
            <*> out3Data3Dflt
        out3Data3Dflt = pure (False)

        -- Evaluation of output 4: level 5
        out4 = outputStream4 enOut4 
            ((.output4) <$> curTagsLevel5) 
            out4Data0 
        (_, out4Data0) = unbundle sw0

        -- Evaluation of output 5: level 6
        out5 = outputStream5 enOut5 
            ((.output5) <$> curTagsLevel6) 
            out5Data0 
        out5Data0 = getMatchingTag 
            <$> out4 
            <*> ((.output4) <$> curTagsLevel6) 
            <*> (pure (0))

        -- Evaluation of output 6: level 6
        out6 = outputStream6 enOut6 
            ((.output6) <$> curTagsLevel6) 
            out6Data0 
        (_, out6Data0) = unbundle sw1

        -- Evaluation of output 7: level 6
        out7 = outputStream7 enOut7 
            ((.output7) <$> curTagsLevel6) 
            out7Data0 
            out7Data1 
        (_, out7Data0) = unbundle sw2
        (_, out7Data1) = unbundle sw3

        -- Evaluation of output 8: level 6
        out8 = outputStream8 enOut8 
            ((.output8) <$> curTagsLevel6) 
            out8Data0 
            out8Data1 
        (_, out8Data0) = unbundle sw4
        (_, out8Data1) = unbundle sw5

        -- Evaluation of output 9: level 6
        out9 = outputStream9 enOut9 
            ((.output9) <$> curTagsLevel6) 
            out9Data0 
        (_, out9Data0) = unbundle sw6

        -- Evaluation of output 10: level 6
        out10 = outputStream10 enOut10 
            ((.output10) <$> curTagsLevel6) 
            out10Data0 
        out10Data0 = getLatestValue 
            <$> input1Win 
            <*> out10Data0Dflt
        out10Data0Dflt = pure (0)

        -- Evaluation of sliding window 0: level 4
        sw0 = slidingWindow0 enSw0 sld0 
            ((.slide0) <$> curTagsLevel4) sw0Data
        sw0Data = getMatchingTag 
            <$> out3 
            <*> ((.output3) <$> curTagsLevel4) 
            <*> (pure False)

        -- Evaluation of sliding window 1: level 5
        sw1 = slidingWindow1 enSw1 sld1 
            ((.slide1) <$> curTagsLevel5) sw1Data
        sw1Data = getMatchingTag 
            <$> input2Win 
            <*> ((.input2) <$> curTagsLevel5) 
            <*> (pure 0)

        -- Evaluation of sliding window 2: level 5
        sw2 = slidingWindow2 enSw2 sld2 
            ((.slide2) <$> curTagsLevel5) sw2Data
        sw2Data = getMatchingTag 
            <$> input2Win 
            <*> ((.input2) <$> curTagsLevel5) 
            <*> (pure 0)

        -- Evaluation of sliding window 3: level 5
        sw3 = slidingWindow3 enSw3 sld3 
            ((.slide3) <$> curTagsLevel5) sw3Data
        sw3Data = getMatchingTag 
            <$> input2Win 
            <*> ((.input2) <$> curTagsLevel5) 
            <*> (pure 0)

        -- Evaluation of sliding window 4: level 5
        sw4 = slidingWindow4 enSw4 sld4 
            ((.slide4) <$> curTagsLevel5) sw4Data
        sw4Data = getMatchingTag 
            <$> input2Win 
            <*> ((.input2) <$> curTagsLevel5) 
            <*> (pure 0)

        -- Evaluation of sliding window 5: level 5
        sw5 = slidingWindow5 enSw5 sld5 
            ((.slide5) <$> curTagsLevel5) sw5Data
        sw5Data = getMatchingTag 
            <$> input2Win 
            <*> ((.input2) <$> curTagsLevel5) 
            <*> (pure 0)

        -- Evaluation of sliding window 6: level 5
        sw6 = slidingWindow6 enSw6 sld6 
            ((.slide6) <$> curTagsLevel5) sw6Data
        sw6Data = getMatchingTag 
            <$> input2Win 
            <*> ((.input2) <$> curTagsLevel5) 
            <*> (pure 0)

        -- Outputing all results: level 7
        output0 = ValidInt <$> output0Data <*> output0Aktv
        output0Data = getMatchingTag 
            <$> out0 
            <*> ((.output0) 
            <$> curTagsLevel7) 
            <*> (pure 0)
        output1 = ValidBool <$> output1Data <*> output1Aktv
        output1Data = getMatchingTag 
            <$> out1 
            <*> ((.output1) 
            <$> curTagsLevel7) 
            <*> (pure False)
        output2 = ValidBool <$> output2Data <*> output2Aktv
        output2Data = getMatchingTag 
            <$> out2 
            <*> ((.output2) 
            <$> curTagsLevel7) 
            <*> (pure False)
        output3 = ValidBool <$> output3Data <*> output3Aktv
        output3Data = getMatchingTag 
            <$> out3 
            <*> ((.output3) 
            <$> curTagsLevel7) 
            <*> (pure False)
        output4 = ValidInt <$> output4Data <*> output4Aktv
        output4Data = getMatchingTag 
            <$> out4 
            <*> ((.output4) 
            <$> curTagsLevel7) 
            <*> (pure 0)
        output5 = ValidBool <$> output5Data <*> output5Aktv
        (_, output5Data) = unbundle out5
        output6 = ValidBool <$> output6Data <*> output6Aktv
        (_, output6Data) = unbundle out6
        output7 = ValidBool <$> output7Data <*> output7Aktv
        (_, output7Data) = unbundle out7
        output8 = ValidBool <$> output8Data <*> output8Aktv
        (_, output8Data) = unbundle out8
        output9 = ValidBool <$> output9Data <*> output9Aktv
        (_, output9Data) = unbundle out9
        output10 = ValidBool <$> output10Data <*> output10Aktv
        (_, output10Data) = unbundle out10

        outputs = Outputs 
            <$> output0 
            <*> output1 
            <*> output2 
            <*> output3 
            <*> output4 
            <*> output5 
            <*> output6 
            <*> output7 
            <*> output8 
            <*> output9 
            <*> output10

        debugSignals = bundle (slides, debugEnables)
        debugEnables = DebugEnables <$>
                            (getPacing <$> enIn0) <*>
                            (getPacing <$> enIn1) <*>
                            (getPacing <$> enIn2) <*>
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
                            (getPacing <$> enOut10)

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


input1Window :: HiddenClockResetEnable dom 
    => Signal dom PacingIn1 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Vec 6 (Tag, Int))
input1Window en tag val = result
    where result = register (repeat (invalidTag, 0)) 
                    (mux (getPacing <$> en) 
                        ((<<+) <$> result <*> (bundle (tag, val)))
                        result)


input2Window :: HiddenClockResetEnable dom 
    => Signal dom PacingIn2 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Vec 5 (Tag, Int))
input2Window en tag val = result
    where result = register (repeat (invalidTag, 0)) 
                    (mux (getPacing <$> en) 
                        ((<<+) <$> result <*> (bundle (tag, val)))
                        result)



outputStream0 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut0 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Vec 6 (Tag, Int))
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
    -> Signal dom (Vec 5 (Tag, Bool))
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
    -> Signal dom (Vec 5 (Tag, Bool))
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
    -> Signal dom (Vec 4 (Tag, Bool))
outputStream3 en tag out1_00 out2_01 out2_10 out1_11 = result
    where
        result = register (repeat (invalidTag, False)) 
                (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = ((out1_00 .&&. out2_01) .||. (out2_10 .&&. out1_11))


outputStream4 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut4 
    -> Signal dom Tag 
    -> Signal dom (Vec 101 Int) 
    -> Signal dom (Vec 2 (Tag, Int))
outputStream4 en tag sw0 = result
    where
        result = register (repeat (invalidTag, 0)) 
                (mux (getPacing <$> en) next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = (merge0 <$> sw0)
        merge0 :: Vec 101 Int -> Int
        merge0 win = fold windowFunc2 (tail win)


outputStream5 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut5 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Tag, Bool)
outputStream5 en tag out4_0 = result
    where
        result = register (invalidTag, False) 
                (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = (out4_0 .>. (5))


outputStream6 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut6 
    -> Signal dom Tag 
    -> Signal dom (Vec 111 Int) 
    -> Signal dom (Tag, Bool)
outputStream6 en tag sw1 = result
    where
        result = register (invalidTag, False) 
                (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = ((merge1 <$> sw1) .<. (1))
        merge1 :: Vec 111 Int -> Int
        merge1 win = fold windowFunc2 (tail win)


outputStream7 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut7 
    -> Signal dom Tag 
    -> Signal dom (Vec 2 Int) 
    -> Signal dom (Vec 2 Int) 
    -> Signal dom (Tag, Bool)
outputStream7 en tag sw2 sw3 = result
    where
        result = register (invalidTag, False) 
                (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = (((merge2 <$> sw2) .<. (15)) .&&. ((merge3 <$> sw3) .>=. (10)))
        merge2 :: Vec 2 Int -> Int
        merge2 win = fold windowFunc2 (tail win)
        merge3 :: Vec 2 Int -> Int
        merge3 win = fold windowFunc2 (tail win)


outputStream8 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut8 
    -> Signal dom Tag 
    -> Signal dom (Vec 2 Int) 
    -> Signal dom (Vec 2 Int) 
    -> Signal dom (Tag, Bool)
outputStream8 en tag sw4 sw5 = result
    where
        result = register (invalidTag, False) 
                (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = (((merge4 <$> sw4) .<. (10)) .&&. ((merge5 <$> sw5) .>=. (5)))
        merge4 :: Vec 2 Int -> Int
        merge4 win = fold windowFunc2 (tail win)
        merge5 :: Vec 2 Int -> Int
        merge5 win = fold windowFunc2 (tail win)


outputStream9 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut9 
    -> Signal dom Tag 
    -> Signal dom (Vec 2 Int) 
    -> Signal dom (Tag, Bool)
outputStream9 en tag sw6 = result
    where
        result = register (invalidTag, False) 
                (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = ((merge6 <$> sw6) .<. (5))
        merge6 :: Vec 2 Int -> Int
        merge6 win = fold windowFunc2 (tail win)


outputStream10 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut10 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Tag, Bool)
outputStream10 en tag in1_0 = result
    where
        result = register (invalidTag, False) 
                (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = (in1_0 .<. (6))



windowFunc0 :: Int -> Bool -> Int
windowFunc0 acc item = acc + 1

windowFunc1 :: Int -> Int -> Int
windowFunc1 acc item = acc + 1

windowFunc2 :: Int -> Int -> Int
windowFunc2 acc item = acc + item


slidingWindow0 :: HiddenClockResetEnable dom 
    => Signal dom PacingOut3 
    -> Signal dom Bool 
    -> Signal dom Tag 
    -> Signal dom Bool 
    -> Signal dom (Tag, (Vec 101 Int)) 
slidingWindow0 newData slide tag inpt = window
    where
        window = register (invalidTag, dflt) (mux en next window)
        next = bundle (tag, nextWindow <$> (snd <$> window) 
                <*> slide <*> (getPacing <$> newData) <*> inpt)
        en = (getPacing <$> newData) .||. slide
        dflt = repeat 0 :: Vec 101 Int

        nextWindow :: Vec 101 Int 
            -> Bool 
            -> Bool 
            -> Bool 
            -> Vec 101 Int
        nextWindow win toSlide newData dta = out
            where
                out = case (toSlide, newData) of
                    (False, False) -> win
                    (False, True) -> lastBucketUpdated
                    (True, False) -> 0 +>> win
                    (True, True) -> 0 +>> lastBucketUpdated
                lastBucketUpdated = 
                    replace 0 (windowFunc0 (head win) dta) win

slidingWindow1 :: HiddenClockResetEnable dom 
    => Signal dom PacingIn2 
    -> Signal dom Bool 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Tag, (Vec 111 Int)) 
slidingWindow1 newData slide tag inpt = window
    where
        window = register (invalidTag, dflt) (mux en next window)
        next = bundle (tag, nextWindow <$> (snd <$> window) 
                <*> slide <*> (getPacing <$> newData) <*> inpt)
        en = (getPacing <$> newData) .||. slide
        dflt = repeat 0 :: Vec 111 Int

        nextWindow :: Vec 111 Int 
            -> Bool 
            -> Bool 
            -> Int 
            -> Vec 111 Int
        nextWindow win toSlide newData dta = out
            where
                out = case (toSlide, newData) of
                    (False, False) -> win
                    (False, True) -> lastBucketUpdated
                    (True, False) -> 0 +>> win
                    (True, True) -> 0 +>> lastBucketUpdated
                lastBucketUpdated = 
                    replace 0 (windowFunc1 (head win) dta) win

slidingWindow2 :: HiddenClockResetEnable dom 
    => Signal dom PacingIn2 
    -> Signal dom Bool 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Tag, (Vec 2 Int)) 
slidingWindow2 newData slide tag inpt = window
    where
        window = register (invalidTag, dflt) (mux en next window)
        next = bundle (tag, nextWindow <$> (snd <$> window) 
                <*> slide <*> (getPacing <$> newData) <*> inpt)
        en = (getPacing <$> newData) .||. slide
        dflt = repeat 0 :: Vec 2 Int

        nextWindow :: Vec 2 Int 
            -> Bool 
            -> Bool 
            -> Int 
            -> Vec 2 Int
        nextWindow win toSlide newData dta = out
            where
                out = case (toSlide, newData) of
                    (False, False) -> win
                    (False, True) -> lastBucketUpdated
                    (True, False) -> 0 +>> win
                    (True, True) -> 0 +>> lastBucketUpdated
                lastBucketUpdated = 
                    replace 0 (windowFunc1 (head win) dta) win

slidingWindow3 :: HiddenClockResetEnable dom 
    => Signal dom PacingIn2 
    -> Signal dom Bool 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Tag, (Vec 2 Int)) 
slidingWindow3 newData slide tag inpt = window
    where
        window = register (invalidTag, dflt) (mux en next window)
        next = bundle (tag, nextWindow <$> (snd <$> window) 
                <*> slide <*> (getPacing <$> newData) <*> inpt)
        en = (getPacing <$> newData) .||. slide
        dflt = repeat 0 :: Vec 2 Int

        nextWindow :: Vec 2 Int 
            -> Bool 
            -> Bool 
            -> Int 
            -> Vec 2 Int
        nextWindow win toSlide newData dta = out
            where
                out = case (toSlide, newData) of
                    (False, False) -> win
                    (False, True) -> lastBucketUpdated
                    (True, False) -> 0 +>> win
                    (True, True) -> 0 +>> lastBucketUpdated
                lastBucketUpdated = 
                    replace 0 (windowFunc1 (head win) dta) win

slidingWindow4 :: HiddenClockResetEnable dom 
    => Signal dom PacingIn2 
    -> Signal dom Bool 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Tag, (Vec 2 Int)) 
slidingWindow4 newData slide tag inpt = window
    where
        window = register (invalidTag, dflt) (mux en next window)
        next = bundle (tag, nextWindow <$> (snd <$> window) 
                <*> slide <*> (getPacing <$> newData) <*> inpt)
        en = (getPacing <$> newData) .||. slide
        dflt = repeat 0 :: Vec 2 Int

        nextWindow :: Vec 2 Int 
            -> Bool 
            -> Bool 
            -> Int 
            -> Vec 2 Int
        nextWindow win toSlide newData dta = out
            where
                out = case (toSlide, newData) of
                    (False, False) -> win
                    (False, True) -> lastBucketUpdated
                    (True, False) -> 0 +>> win
                    (True, True) -> 0 +>> lastBucketUpdated
                lastBucketUpdated = 
                    replace 0 (windowFunc1 (head win) dta) win

slidingWindow5 :: HiddenClockResetEnable dom 
    => Signal dom PacingIn2 
    -> Signal dom Bool 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Tag, (Vec 2 Int)) 
slidingWindow5 newData slide tag inpt = window
    where
        window = register (invalidTag, dflt) (mux en next window)
        next = bundle (tag, nextWindow <$> (snd <$> window) 
                <*> slide <*> (getPacing <$> newData) <*> inpt)
        en = (getPacing <$> newData) .||. slide
        dflt = repeat 0 :: Vec 2 Int

        nextWindow :: Vec 2 Int 
            -> Bool 
            -> Bool 
            -> Int 
            -> Vec 2 Int
        nextWindow win toSlide newData dta = out
            where
                out = case (toSlide, newData) of
                    (False, False) -> win
                    (False, True) -> lastBucketUpdated
                    (True, False) -> 0 +>> win
                    (True, True) -> 0 +>> lastBucketUpdated
                lastBucketUpdated = 
                    replace 0 (windowFunc1 (head win) dta) win

slidingWindow6 :: HiddenClockResetEnable dom 
    => Signal dom PacingIn2 
    -> Signal dom Bool 
    -> Signal dom Tag 
    -> Signal dom Int 
    -> Signal dom (Tag, (Vec 2 Int)) 
slidingWindow6 newData slide tag inpt = window
    where
        window = register (invalidTag, dflt) (mux en next window)
        next = bundle (tag, nextWindow <$> (snd <$> window) 
                <*> slide <*> (getPacing <$> newData) <*> inpt)
        en = (getPacing <$> newData) .||. slide
        dflt = repeat 0 :: Vec 2 Int

        nextWindow :: Vec 2 Int 
            -> Bool 
            -> Bool 
            -> Int 
            -> Vec 2 Int
        nextWindow win toSlide newData dta = out
            where
                out = case (toSlide, newData) of
                    (False, False) -> win
                    (False, True) -> lastBucketUpdated
                    (True, False) -> 0 +>> win
                    (True, True) -> 0 +>> lastBucketUpdated
                lastBucketUpdated = 
                    replace 0 (windowFunc1 (head win) dta) win




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
