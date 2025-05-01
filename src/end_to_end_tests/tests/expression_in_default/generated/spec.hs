module Spec where

import Clash.Prelude

---------------------------------------------------------------

-- input x : Int
-- input y : Int
-- 
-- output a := x.offset(by: -1).defaults(to: c)
-- output b := a.offset(by: -1).defaults(to: a) + y.offset(by: -1).defaults(to: c + x + y)
-- output c := y.offset(by: -1).defaults(to: y)
-- 

---------------------------------------------------------------

-- Evaluation Order
-- x, c, y
-- a
-- b

-- Memory Window
-- window x = 2
-- window c = 2
-- window y = 1
-- window a = 1
-- window b = 1

-- Pipeline Visualization
-- x,c,y | x,c,y | x,c,y | x,c,y | x,c,y | x,c,y | x,c,y | x,c,y | x,c,y | x,c,y
-- -----------------------------------------------------------------------------
--       | a     | a     | a     | a     | a     | a     | a     | a     | a    
-- -----------------------------------------------------------------------------
--       |       | b     | b     | b     | b     | b     | b     | b     | b    
-- -----------------------------------------------------------------------------

-- input0 = x
-- input1 = y
-- output0 = a
-- output1 = b
-- output2 = c

---------------------------------------------------------------


type HasInput0 = (Int, Bool)
type HasInput1 = (Int, Bool)
type Inputs = (HasInput0, HasInput1)

type HasOutput0 = (Int, Bool)
type HasOutput1 = (Int, Bool)
type HasOutput2 = (Int, Bool)
type Outputs = (HasOutput0, HasOutput1, HasOutput2)

type Pacings = (Bool, Bool, Bool)

type Event = (Inputs, Pacings)

nullEvent :: Event
nullEvent = (((0, False), (0, False)), (False, False, False))


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
        newEvent = pacing0 .||. pacing1 .||. pacing2
        event = bundle (inputs, pacings)

        pacings = bundle (pacing0, pacing1, pacing2)

        (input0, input1) = unbundle inputs
        (_, hasInput0) = unbundle input0
        (_, hasInput1) = unbundle input1

        pacing0 = hasInput0 .&&. hasInput1
        pacing1 = hasInput0 .&&. hasInput1
        pacing2 = hasInput1





---------------------------------------------------------------

type Tag = Unsigned 8
-- maxTag must be at least the size of the sliding window to avoid duplicate tags in the window
-- also to avoid having to do modulo operations maxTag must be at least as big as the largest offset
maxTag = 2 :: Tag
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

earlierTag :: Tag -> Tag -> Tag
earlierTag curTag cyclesBefore = if curTag > cyclesBefore then curTag - cyclesBefore else curTag - cyclesBefore + maxTag
     

llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom ((Bool, Outputs), ((Bool, Bool, Bool)))
llc event = bundle (bundle (toPop, outputs), debugSignals)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        toPop = pure True

        (inputs, pacings) = unbundle poppedEvent
        (input0, input1) = unbundle inputs
        (_, input0HasData) = unbundle input0
        (_, input1HasData) = unbundle input1
        (p0, p1, p2) = unbundle pacings

        tagIn0 = genTag input0HasData
        tagOut2 = genTag p2
        tagIn1 = genTag input1HasData
        tagOut0 = genTag p0
        tagOut1 = genTag p1

        -- tag generation takes 1 cycle so we need to delay the input data
        (input0Data, _) = unbundle (delay (0, False) input0)
        (input1Data, _) = unbundle (delay (0, False) input1)

        -- delayed tags to be used in different levels 
        tagsDefault = (invalidTag, invalidTag, invalidTag, invalidTag, invalidTag)
        curTags = bundle (tagIn0, tagIn1, tagOut0, tagOut1, tagOut2)
        curTagsLevel1 = delay tagsDefault curTags
        curTagsLevel2 = delay tagsDefault (delay tagsDefault curTags)
        curTagsLevel3 = delay tagsDefault (delay tagsDefault (delay tagsDefault curTags))

        enIn0 = delay False input0HasData
        enOut2 = delay False p2
        enIn1 = delay False input1HasData
        enOut0 = delay False (delay False p0)
        enOut1 = delay False (delay False (delay False p1))

        output0Aktv = delay False (delay False (delay False (delay False p0)))
        output1Aktv = delay False (delay False (delay False (delay False p1)))
        output2Aktv = delay False (delay False (delay False (delay False p2)))

        -- level 0
        input0Win = input0Window enIn0 (bundle (tagIn0, input0Data))
        input1Win = input1Window enIn1 (bundle (tagIn1, input1Data))

        -- level 1
        (out0Level1TagIn0, _, _, _, _) = unbundle curTagsLevel1
        out0 = outputStream0 enOut0 out0Data0 
        out0Data0 = getOffset <$> input0Win <*> out0Level1TagIn0 <*> (pure 1) <*> (pure 2000)

        -- level 2
        (_, out1Level2TagIn1, out1Level2TagOut0, _, _) = unbundle curTagsLevel2
        out1 = outputStream1 enOut1 out1Data0 out1Data1 
        out1Data0 = getOffsetFromNonVec <$> input1Win <*> out1Level2TagIn1 <*> (pure 1) <*> (pure 1000)
        out1Data1 = getOffsetFromNonVec <$> out0 <*> out1Level2TagOut0 <*> (pure 1) <*> (pure 2000)

        -- level 0
        out2 = outputStream2 enOut2 out2Data0 
        out2Data0 = getOffsetFromNonVec <$> input1Win <*> tagIn1 <*> (pure 1) <*> (pure 2000)

        -- level 3
        (_, _, level3TagOut0, level3TagOut1, level3TagOut2) = unbundle curTagsLevel3
        output0 = bundle (output0Data, output0Aktv)
        (_, output0Data) = unbundle out0
        output1 = bundle (output1Data, output1Aktv)
        (_, output1Data) = unbundle out1
        output2 = bundle (output2Data, output2Aktv)
        (_, output2Data) = unbundle (getMatchingTag <$> out2 <*> level3TagOut2 <*> (pure 0))

        outputs = bundle (output0, output1, output2)

        debugSignals = pacings

        genTag :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag
        genTag en = t
            where 
                t = register 1 (mux en next_t t)
                next_t = mux (t .==. (pure maxTag)) (pure 1) (t + 1)





input0Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
input0Window en td = result
    where result = register (repeat (invalidTag, 0)) (mux en ((<<+) <$> result <*> td) result)


input1Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
input1Window en td = result
    where result = register (invalidTag, 0) (mux en td result)



outputStream0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream0 en in0WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0
        (tag, in0) = unbundle in0WithTag


outputStream1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream1 en in1WithTag out0WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = out0 + in1
        (tag, in1) = unbundle in1WithTag
        (_, out0) = unbundle out0WithTag


outputStream2 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Vec 2 (Tag, Int))
outputStream2 en in1WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in1
        (tag, in1) = unbundle in1WithTag






---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Outputs, (QPush, QPop, QPushValid, QPopValid, (Bool, Bool, Bool)))
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
    Signal TestDomain Inputs -> Signal TestDomain (Outputs, (QPush, QPop, QPushValid, QPopValid, (Bool, Bool, Bool)))
topEntity clk rst en inputs = exposeClockResetEnable (monitor inputs) clk rst en
