module VerySimple where

import Clash.Prelude

---------------------------------------------------------------

-- input x : Int
-- input y : Int
-- 
-- output a := x + y
-- output b @1kHz := a.aggregate(over: 0.003s, using: sum)

---------------------------------------------------------------

-- Evaluation Order
-- x, y
-- a
-- sw(a,b)
-- b

-- Memory Window
-- window x = 1
-- window y = 1
-- window a = 1
-- window sw(a,b) = 1
-- window b = 1

-- Pipeline Visualization
-- x,y     | x,y     | x,y     | x,y     | x,y     | x,y     | x,y     | x,y     | x,y     | x,y    
-- -------------------------------------------------------------------------------------------------
--         | a       | a       | a       | a       | a       | a       | a       | a       | a      
-- -------------------------------------------------------------------------------------------------
--         |         | sw(a,b) | sw(a,b) | sw(a,b) | sw(a,b) | sw(a,b) | sw(a,b) | sw(a,b) | sw(a,b)
-- -------------------------------------------------------------------------------------------------
--         |         |         | b       | b       | b       | b       | b       | b       | b      
-- -------------------------------------------------------------------------------------------------

-- input0 = x
-- input1 = y
-- output0 = a
-- output1 = b
-- sw0 = sw(a,b)

---------------------------------------------------------------


type HasInput0 = (Int, Bool)
type HasInput1 = (Int, Bool)
type Inputs = (HasInput0, HasInput1)

type HasOutput0 = (Int, Bool)
type HasOutput1 = (Int, Bool)
type Outputs = (HasOutput0, HasOutput1)

type Pacings = (Bool, Bool)
type Slides = Bool

type Event = (Inputs, Slides, Pacings)

nullEvent :: Event
nullEvent = (((0, False), (0, False)), False, (False, False))


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


hlc :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Bool, Event)
hlc inputs = out
    where 
        out = bundle (newEvent, event)
        newEvent = slide0 .||. pacing0 .||. pacing1
        event = bundle (inputs, slides, pacings)

        slides = slide0
        pacings = bundle (pacing0, pacing1)

        (input0, input1) = unbundle inputs
        (_, hasInput0) = unbundle input0
        (_, hasInput1) = unbundle input1

        pacing0 = hasInput0 .&&. hasInput1
        pacing1 = timer0Over

        slide0 = timer0Over

        timer0Over = timer0 .>=. period0InNs
        timer0 = timer timer0Over
        period0InNs = 1000000

        timer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int
        timer reset = register 0 (mux reset (pure deltaTime) nextTime)
            where 
                nextTime = timer reset + pure deltaTime
                deltaTime = systemClockPeriodNs


---------------------------------------------------------------

type Tag = Unsigned 8
-- maxTag must be at least the size of the sliding window to avoid duplicate tags in the window
-- default arbitrary value of 5 otherwise
maxTag = 3 :: Tag
invalidTag = maxTag + 1

getOffset :: KnownNat n => Vec n (Tag, a) -> Tag -> Tag -> a -> (Tag, a)
getOffset win tag offset dflt = out
    where 
        offsetTag = if tag > offset then tag - offset else tag - offset + maxTag
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
        offsetTag = if tag > offset then tag - offset else tag - offset + maxTag
        out = if offsetTag == winTag then (tag, winData) else (tag, dflt)


llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom ((Bool, Outputs), Tag)
llc event = bundle (bundle (toPop, outputs), debugSignals)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        toPop = pure True

        (inputs, slides, pacings) = unbundle poppedEvent
        (input0, input1) = unbundle inputs
        (input0Data, input0HasData) = unbundle input0
        (input1Data, input1HasData) = unbundle input1
        slide0 = slides
        (p0, p1) = unbundle pacings

        outputs = bundle (output0, output1)

        tag = genTag (p0 .||. p1)

        in0Tag = tag
        in1Tag = tag
        out0Tag = delay invalidTag tag
        sw0Tag = delay invalidTag (delay invalidTag tag)
        out1Tag = delay invalidTag (delay invalidTag (delay invalidTag tag))

        enIn0 = input0HasData
        enIn1 = input1HasData
        enOut0 = delay False p0
        enSw0 = delay False (delay False slide0 .||. p1)
        sld0 = delay False (delay False slide0)
        sw0DataPacing = delay False (delay False p0)
        enOut1 = delay False (delay False (delay False p1))

        outputPhaseTag = delay invalidTag (delay invalidTag (delay invalidTag (delay invalidTag tag)))
        output0Aktv = delay False (delay False (delay False (delay False p0)))
        output1Aktv = delay False (delay False (delay False (delay False p1)))

        output0 = bundle (output0Data, output0Aktv)
        (_, output0Data) = unbundle out0
        output1 = bundle (output1Data, output1Aktv)
        (_, output1Data) = unbundle out1

        input0Win = input0Window enIn0 (bundle (in0Tag, input0Data))
        input1Win = input1Window enIn1 (bundle (in1Tag, input1Data))

        out0 = outputStream0 enOut0 out0Data0 out0Data1 
        out0Data0 = input0Win
        out0Data1 = input1Win

        out1 = outputStream1 enOut1 out1Data0 
        out1Data0 = sw0

        sw0 = slidingWindow0 enSw0 sld0 (bundle (sw0Tag, sw0Data))
        sw0Data = bundle (sw0DataVal, sw0DataPacing)
        (_, sw0DataVal) = unbundle out0

        debugSignals = tag

        genTag :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag
        genTag en = t
            where 
                t = register 1 (mux en next_t t)
                next_t = mux (t .==. (pure maxTag)) (pure 1) (t + 1)





input0Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
input0Window en td = result
    where result = register (invalidTag, 0) (mux en td result)


input1Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
input1Window en td = result
    where result = register (invalidTag, 0) (mux en td result)



outputStream0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream0 en in0WithTag in1WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + in1
        (tag, in0) = unbundle in0WithTag
        (_, in1) = unbundle in1WithTag


outputStream1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, (Vec 3 Int)) -> Signal dom (Tag, Int)
outputStream1 en sw0WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = (merge0 <$> sw0)
        (tag, sw0) = unbundle sw0WithTag
        merge0 :: Vec 3 Int -> Int
        merge0 win = fold windowBucketFunc0 win



windowBucketFunc0 :: Int -> Int -> Int
windowBucketFunc0 acc item = acc + item


slidingWindow0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom (Tag, (Int, Bool)) -> Signal dom (Tag, (Vec 3 Int)) 
slidingWindow0 en slide hasInputWithTag = window
    where
        window = register (invalidTag, dflt) (mux en next window)
        next = bundle (tag, nextWindow <$> (snd <$> window) <*> slide <*> hasInput)
        dflt = repeat 0 :: Vec 3 Int
        (tag, hasInput) = unbundle hasInputWithTag

        nextWindow :: Vec 3 Int -> Bool -> (Int, Bool) -> Vec 3 Int
        nextWindow win toSlide inpt = out
            where
                (dta, hasData) = inpt
                out = case (toSlide, hasData) of
                    (False, False) -> win
                    (False, True) -> lastBucketUpdated
                    (True, False) -> win <<+ 0
                    (True, True) -> win <<+ dta
                lastBucketUpdated = replace lastIndx (windowBucketFunc0 (last win) dta) win
                lastIndx = length win - 1       




---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Outputs, (Tag, QPush, QPop, QPushValid, QPopValid))
monitor inputs = bundle (outputs, debugSignals)
    where 
        (newEvent, event) = unbundle (hlc inputs)

        (qPushValid, qPopValid, qPopData) = unbundle (queue (bundle (qPush, qPop, qInptData)))
        qPush = newEvent
        qPop = toPop
        qInptData = event

        (llcOutput, llcDebug) = unbundle (llc (bundle (qPopValid, qPopData)))
        (toPop, outputs) = unbundle llcOutput

        llcTag = llcDebug
        debugSignals = bundle (llcTag, qPush, qPop, qPushValid, qPopValid)


---------------------------------------------------------------

topEntity :: Clock TestDomain -> Reset TestDomain -> Enable TestDomain -> 
    Signal TestDomain Inputs -> Signal TestDomain (Outputs, (Tag, QPush, QPop, QPushValid, QPopValid))
topEntity clk rst en inputs = exposeClockResetEnable (monitor inputs) clk rst en
