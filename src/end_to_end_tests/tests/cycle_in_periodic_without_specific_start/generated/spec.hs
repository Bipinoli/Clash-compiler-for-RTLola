{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Spec where

import Clash.Prelude

---------------------------------------------------------------

-- output a @1kHz := c.offset(by: -1).defaults(to: 0) + 1
-- output b @1kHz := a.offset(by: -1).defaults(to: 0)
-- output c @1kHz := b.offset(by: -1).defaults(to: 0)   

---------------------------------------------------------------

-- Evaluation Order
-- a, b, c

-- Memory Window
-- window b = 1
-- window c = 1
-- window a = 1

-- Pipeline Visualization
-- a,b,c | a,b,c | a,b,c | a,b,c | a,b,c | a,b,c | a,b,c | a,b,c | a,b,c | a,b,c
-- -----------------------------------------------------------------------------

-- output0 = a
-- output1 = b
-- output2 = c

---------------------------------------------------------------

data ValidInt = ValidInt {
    value :: Int,
    valid :: Bool
} deriving (Generic, NFDataX)



data Outputs = Outputs {
    output0 :: ValidInt,
    output1 :: ValidInt,
    output2 :: ValidInt
} deriving (Generic, NFDataX)


class Pacing a where getPacing :: a -> Bool

data PacingOut0 = PacingOut0 Bool deriving (Generic, NFDataX)
data PacingOut1 = PacingOut1 Bool deriving (Generic, NFDataX)
data PacingOut2 = PacingOut2 Bool deriving (Generic, NFDataX)

instance Pacing PacingOut0 where getPacing (PacingOut0 x) = x
instance Pacing PacingOut1 where getPacing (PacingOut1 x) = x
instance Pacing PacingOut2 where getPacing (PacingOut2 x) = x

data Pacings = Pacings {
    pacingOut0 :: PacingOut0,
    pacingOut1 :: PacingOut1,
    pacingOut2 :: PacingOut2
} deriving (Generic, NFDataX)

data DebugEnables = DebugEnables {
    enableOut0 :: Bool,
    enableOut1 :: Bool,
    enableOut2 :: Bool
} deriving (Generic, NFDataX)


type Tag = Unsigned 8

data Tags = Tags {
    output0 :: Tag,
    output1 :: Tag,
    output2 :: Tag
} deriving (Generic, NFDataX)

type Event = (Pacings)

nullEvent :: Event
nullEvent = (nullPacings)
nullPacings = Pacings nullPacingOut0 nullPacingOut1 nullPacingOut2 
nullPacingOut0 = PacingOut0 False 
nullPacingOut1 = PacingOut1 False 
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


hlc :: HiddenClockResetEnable dom => Signal dom (Bool, Event)
hlc = out
    where 
        out = bundle (newEvent, event)
        newEvent = timer0Over

        event = (pacings)

        pacings = Pacings <$> pOut0 <*> pOut1 <*> pOut2


        pOut0 = PacingOut0 <$> timer0Over
        pOut1 = PacingOut1 <$> timer0Over
        pOut2 = PacingOut2 <$> timer0Over


        timer0Over = timer0 .>=. period0InNs
        timer0 = timer timer0Over
        period0InNs = 1000000

        timer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int
        timer reset = register 0 (mux reset (pure deltaTime) nextTime)
            where 
                nextTime = timer reset + pure deltaTime
                deltaTime = systemClockPeriodNs


---------------------------------------------------------------

-- maxTag must be at least the size of the maximum window to avoid duplicate tags in the window
-- also to avoid having to do modulo operations maxTag must be at least as big as the largest offset
maxTag = 2 :: Tag
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

        toPop = pure True

        pacings = poppedEvent



        pOut0 = (.pacingOut0) <$> pacings
        pOut1 = (.pacingOut1) <$> pacings
        pOut2 = (.pacingOut2) <$> pacings
        
        tOut0 = genTag (getPacing <$> pOut0)
        tOut1 = genTag (getPacing <$> pOut1)
        tOut2 = genTag (getPacing <$> pOut2)

        -- tag generation takes 1 cycle so we need to delay the input data

        -- delayed tags to be used in different levels 
        tagsDefault = Tags nullT nullT nullT 
        curTags = Tags <$> tOut0 <*> tOut1 <*> tOut2
        curTagsLevel1 = delayFor d1 tagsDefault curTags
        nullT = invalidTag

        enOut0 = delayFor d1 nullPacingOut0 pOut0
        enOut1 = delayFor d1 nullPacingOut1 pOut1
        enOut2 = delayFor d1 nullPacingOut2 pOut2

        output0Aktv = delayFor d2 False (getPacing <$> pOut0)
        output1Aktv = delayFor d2 False (getPacing <$> pOut1)
        output2Aktv = delayFor d2 False (getPacing <$> pOut2)

        -- Evaluation of input windows: level 0

        -- Evaluation of output 0: level 0
        out0 = outputStream0 enOut0 tOut0 out0Data0 
        out0Data0 = getOffsetFromNonVec <$> out2 <*> tOut2 <*> (pure 1) <*> out0Data0Dflt
        out0Data0Dflt = pure (0)

        -- Evaluation of output 1: level 0
        out1 = outputStream1 enOut1 tOut1 out1Data0 
        out1Data0 = getOffsetFromNonVec <$> out0 <*> tOut0 <*> (pure 1) <*> out1Data0Dflt
        out1Data0Dflt = pure (0)

        -- Evaluation of output 2: level 0
        out2 = outputStream2 enOut2 tOut2 out2Data0 
        out2Data0 = getOffsetFromNonVec <$> out1 <*> tOut1 <*> (pure 1) <*> out2Data0Dflt
        out2Data0Dflt = pure (0)

        -- Outputing all results: level 1
        output0 = ValidInt <$> output0Data <*> output0Aktv
        (_, output0Data) = unbundle out0
        output1 = ValidInt <$> output1Data <*> output1Aktv
        (_, output1Data) = unbundle out1
        output2 = ValidInt <$> output2Data <*> output2Aktv
        (_, output2Data) = unbundle out2

        outputs = Outputs <$> output0 <*> output1 <*> output2

        debugSignals = debugEnables
        debugEnables = DebugEnables <$>
                            (getPacing <$> enOut0) <*>
                            (getPacing <$> enOut1) <*>
                            (getPacing <$> enOut2)

        genTag :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag
        genTag en = t
            where 
                t = register 1 (mux en next_t t)
                next_t = mux (t .==. (pure maxTag)) (pure 1) (t + 1)






outputStream0 :: HiddenClockResetEnable dom => Signal dom PacingOut0 -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
outputStream0 en tag out2_0 = result
    where
        result = register (invalidTag, 0) (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out2_0 + 1


outputStream1 :: HiddenClockResetEnable dom => Signal dom PacingOut1 -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
outputStream1 en tag out0_ = result
    where
        result = register (invalidTag, 0) (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out0_


outputStream2 :: HiddenClockResetEnable dom => Signal dom PacingOut2 -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
outputStream2 en tag out1_ = result
    where
        result = register (invalidTag, 0) (mux (getPacing <$> en) nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out1_







---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom =>  Signal dom (Outputs, (QPush, QPop, QPushValid, QPopValid, DebugEnables))
monitor = bundle (outputs, debugSignals)
    where 
        (newEvent, event) = unbundle (hlc)

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
    Signal TestDomain (Outputs, (QPush, QPop, QPushValid, QPopValid, DebugEnables))
topEntity clk rst en = exposeClockResetEnable (monitor ) clk rst en
