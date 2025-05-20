{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Spec where

import Clash.Prelude

---------------------------------------------------------------

-- input x: Int
-- input y: Int
-- 
-- output a := x + 1
-- output b @1kHz := y.hold(or: 1) + d.hold(or: 2)
-- output c @2kHz := b.hold(or: 10) + a.hold(or: 11)
-- output d @(x & y) := c.hold(or: 0) + 1
-- 
-- output e @1kHz := e.offset(by: -1).defaults(to: 0) + 1
-- output f := e + 1
-- output g := f + 1 

---------------------------------------------------------------

-- Evaluation Order
-- x, d, y, e
-- a, f
-- b, g
-- c

-- Memory Window
-- window c = 1
-- window a = 1
-- window d = 1
-- window g = 1
-- window y = 1
-- window b = 1
-- window f = 1
-- window x = 1
-- window e = 1

-- Pipeline Visualization
-- x,d,y,e |         |         |         | x,d,y,e |         |         |         | x,d,y,e |        
-- -------------------------------------------------------------------------------------------------
--         | a,f     |         |         |         | a,f     |         |         |         | a,f    
-- -------------------------------------------------------------------------------------------------
--         |         | b,g     |         |         |         | b,g     |         |         |        
-- -------------------------------------------------------------------------------------------------
--         |         |         | c       |         |         |         | c       |         |        
-- -------------------------------------------------------------------------------------------------

-- input0 = x
-- input1 = y
-- output0 = a
-- output1 = b
-- output2 = c
-- output3 = d
-- output4 = e
-- output5 = f
-- output6 = g

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
    output6 :: ValidInt
} deriving (Generic, NFDataX)

data Pacings = Pacings {
    pacing0 :: Bool,
    pacing1 :: Bool,
    pacing2 :: Bool,
    pacing3 :: Bool,
    pacing4 :: Bool,
    pacing5 :: Bool,
    pacing6 :: Bool
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
    output6 :: Tag
} deriving (Generic, NFDataX)

type Event = (Inputs, Pacings)

nullEvent :: Event
nullEvent = (nullInputs, nullPacings)
nullInputs = Inputs (ValidInt 0 False) (ValidInt 0 False) 
nullPacings = Pacings False False False False False False False 

---------------------------------------------------------------

type QMemSize = 5

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
        newEvent = hasInput0 .||. hasInput1 .||. p1 .||. p2 .||. p4 .||. p5 .||. p6

        event = bundle (inputs, pacings)

        pacings = Pacings <$> p0 <*> p1 <*> p2 <*> p3 <*> p4 <*> p5 <*> p6

        hasInput0 = ((.valid). (.input0)) <$> inputs
        hasInput1 = ((.valid). (.input1)) <$> inputs

        p0 = hasInput0
        p1 = timer1Over
        p2 = timer0Over
        p3 = hasInput0 .&&. hasInput1
        p4 = timer1Over
        p5 = timer1Over
        p6 = timer1Over


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
     

llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom ((Bool, Outputs), (Pacings))
llc event = bundle (bundle (toPop, outputs), debugSignals)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        isPipelineReady = pipelineReady startNewPipeline
        startNewPipeline = mux (isPipelineReady .&&. isValidEvent) (pure True) (pure False)
        toPop = isPipelineReady .&&. not <$> startNewPipeline

        (inputs, pacings) = unbundle poppedEvent

        input0 = (.input0) <$> inputs
        input1 = (.input1) <$> inputs
        input0HasData = ((.valid). (.input0)) <$> inputs
        input1HasData = ((.valid). (.input1)) <$> inputs


        p0 = (.pacing0) <$> pacings
        p1 = (.pacing1) <$> pacings
        p2 = (.pacing2) <$> pacings
        p3 = (.pacing3) <$> pacings
        p4 = (.pacing4) <$> pacings
        p5 = (.pacing5) <$> pacings
        p6 = (.pacing6) <$> pacings
        
        tIn0 = genTag input0HasData
        tOut3 = genTag p3
        tIn1 = genTag input1HasData
        tOut4 = genTag p4
        tOut0 = genTag p0
        tOut5 = genTag p5
        tOut1 = genTag p1
        tOut6 = genTag p6
        tOut2 = genTag p2

        -- tag generation takes 1 cycle so we need to delay the input data
        input0Data = delay 0 (((.value). (.input0)) <$> inputs)
        input1Data = delay 0 (((.value). (.input1)) <$> inputs)

        -- delayed tags to be used in different levels 
        tagsDefault = Tags nullT nullT nullT nullT nullT nullT nullT nullT nullT 
        curTags = Tags <$> tIn0 <*> tIn1 <*> tOut0 <*> tOut1 <*> tOut2 <*> tOut3 <*> tOut4 <*> tOut5 <*> tOut6
        curTagsLevel1 = delayFor d1 tagsDefault curTags
        curTagsLevel2 = delayFor d2 tagsDefault curTags
        curTagsLevel3 = delayFor d3 tagsDefault curTags
        curTagsLevel4 = delayFor d4 tagsDefault curTags
        nullT = invalidTag

        enIn0 = delayFor d1 False input0HasData
        enOut3 = delayFor d1 False p3
        enIn1 = delayFor d1 False input1HasData
        enOut4 = delayFor d1 False p4
        enOut0 = delayFor d2 False p0
        enOut5 = delayFor d2 False p5
        enOut1 = delayFor d3 False p1
        enOut6 = delayFor d3 False p6
        enOut2 = delayFor d4 False p2

        output0Aktv = delayFor d5 False p0
        output1Aktv = delayFor d5 False p1
        output2Aktv = delayFor d5 False p2
        output3Aktv = delayFor d5 False p3
        output4Aktv = delayFor d5 False p4
        output5Aktv = delayFor d5 False p5
        output6Aktv = delayFor d5 False p6

        -- Evaluation of input windows: level 0
        input0Win = input0Window enIn0 tIn0 input0Data
        input1Win = input1Window enIn1 tIn1 input1Data

        -- Evaluation of output 0: level 1
        out0 = outputStream0 enOut0 ((.output0) <$> curTagsLevel1) out0Data0 
        out0Data0 = getMatchingTagFromNonVec <$> input0Win <*> ((.input0) <$> curTagsLevel1) <*> (pure (0))

        -- Evaluation of output 1: level 2
        out1 = outputStream1 enOut1 ((.output1) <$> curTagsLevel2) out1Data0 out1Data1 
        out1Data0 = getLatestValueFromNonVec <$> input1Win <*> out1Data0Dflt
        out1Data0Dflt = pure (1)
        out1Data1 = getLatestValueFromNonVec <$> out3 <*> out1Data1Dflt
        out1Data1Dflt = pure (2)

        -- Evaluation of output 2: level 3
        out2 = outputStream2 enOut2 ((.output2) <$> curTagsLevel3) out2Data0 out2Data1 
        out2Data0 = getLatestValueFromNonVec <$> out0 <*> out2Data0Dflt
        out2Data0Dflt = pure (11)
        out2Data1 = getLatestValueFromNonVec <$> out1 <*> out2Data1Dflt
        out2Data1Dflt = pure (10)

        -- Evaluation of output 3: level 0
        out3 = outputStream3 enOut3 tOut3 out3Data0 
        out3Data0 = getLatestValueFromNonVec <$> out2 <*> out3Data0Dflt
        out3Data0Dflt = pure (0)

        -- Evaluation of output 4: level 0
        out4 = outputStream4 enOut4 tOut4 out4Data0 
        out4Data0 = getOffsetFromNonVec <$> out4 <*> tOut4 <*> (pure 1) <*> out4Data0Dflt
        out4Data0Dflt = pure (0)

        -- Evaluation of output 5: level 1
        out5 = outputStream5 enOut5 ((.output5) <$> curTagsLevel1) out5Data0 
        out5Data0 = getMatchingTagFromNonVec <$> out4 <*> ((.output4) <$> curTagsLevel1) <*> (pure (0))

        -- Evaluation of output 6: level 2
        out6 = outputStream6 enOut6 ((.output6) <$> curTagsLevel2) out6Data0 
        out6Data0 = getMatchingTagFromNonVec <$> out5 <*> ((.output5) <$> curTagsLevel2) <*> (pure (0))

        -- Outputing all results: level 4
        output0 = ValidInt <$> output0Data <*> output0Aktv
        (_, output0Data) = unbundle out0
        output1 = ValidInt <$> output1Data <*> output1Aktv
        (_, output1Data) = unbundle out1
        output2 = ValidInt <$> output2Data <*> output2Aktv
        (_, output2Data) = unbundle out2
        output3 = ValidInt <$> output3Data <*> output3Aktv
        (_, output3Data) = unbundle out3
        output4 = ValidInt <$> output4Data <*> output4Aktv
        (_, output4Data) = unbundle out4
        output5 = ValidInt <$> output5Data <*> output5Aktv
        (_, output5Data) = unbundle out5
        output6 = ValidInt <$> output6Data <*> output6Aktv
        (_, output6Data) = unbundle out6

        outputs = Outputs <$> output0 <*> output1 <*> output2 <*> output3 <*> output4 <*> output5 <*> output6

        debugSignals = pacings

        genTag :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag
        genTag en = t
            where 
                t = register 1 (mux en next_t t)
                next_t = mux (t .==. (pure maxTag)) (pure 1) (t + 1)


pipelineReady :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
pipelineReady rst = toWait .==. pure 0 
    where 
        waitTime = pure 3 :: Signal dom Int
        toWait = register (0 :: Int) next
        next = mux rst waitTime (mux (toWait .>. pure 0) (toWait - 1) toWait)



input0Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
input0Window en tag val = result
    where result = register (invalidTag, 0) (mux en (bundle (tag, val)) result)


input1Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
input1Window en tag val = result
    where result = register (invalidTag, 0) (mux en (bundle (tag, val)) result)



outputStream0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
outputStream0 en tag in0 = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + 1


outputStream1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom Int -> Signal dom (Tag, Int)
outputStream1 en tag in1 out3 = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in1 + out3


outputStream2 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom Int -> Signal dom (Tag, Int)
outputStream2 en tag out0 out1 = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out1 + out0


outputStream3 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
outputStream3 en tag out2 = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out2 + 1


outputStream4 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
outputStream4 en tag out4 = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out4 + 1


outputStream5 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
outputStream5 en tag out4 = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out4 + 1


outputStream6 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag -> Signal dom Int -> Signal dom (Tag, Int)
outputStream6 en tag out5 = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out5 + 1







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
