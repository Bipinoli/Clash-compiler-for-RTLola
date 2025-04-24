module VerySimple where

import Clash.Prelude

---------------------------------------------------------------

-- input x : Int
-- input y : Int
-- 
-- output a := x.offset(by: -2).defaults(to: 10) + b.offset(by: -3).defaults(to: 100)
-- output b := x + y

---------------------------------------------------------------

-- Evaluation Order
-- x, y, a
-- b

-- Memory Window
-- window x = 2
-- window y = 1
-- window a = 1
-- window b = 3

-- Pipeline Visualization
-- x,y,a | x,y,a | x,y,a | x,y,a | x,y,a | x,y,a | x,y,a | x,y,a | x,y,a | x,y,a
-- -----------------------------------------------------------------------------
--       | b     | b     | b     | b     | b     | b     | b     | b     | b    
-- -----------------------------------------------------------------------------

-- input0 = x
-- input1 = y
-- output0 = a
-- output1 = b

---------------------------------------------------------------


type HasInput0 = (Int, Bool)
type HasInput1 = (Int, Bool)
type Inputs = (HasInput0, HasInput1)

type HasOutput0 = (Int, Bool)
type HasOutput1 = (Int, Bool)
type Outputs = (HasOutput0, HasOutput1)

type Pacings = (Bool, Bool)

type Event = (Inputs, Pacings)

nullEvent :: Event
nullEvent = (((0, False), (0, False)), (False, False))


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
        newEvent = pacing0 .||. pacing1
        event = bundle (inputs, pacings)

        pacings = bundle (pacing0, pacing1)

        (input0, input1) = unbundle inputs
        (_, hasInput0) = unbundle input0
        (_, hasInput1) = unbundle input1

        pacing0 = hasInput0 .&&. hasInput1
        pacing1 = hasInput0 .&&. hasInput1





---------------------------------------------------------------

type Tag = Unsigned 8
-- maxTag must be at least the size of the sliding window to avoid duplicate tags in the window
-- default arbitrary value of 5 otherwise
maxTag = 5 :: Tag
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

        (inputs, pacings) = unbundle poppedEvent
        (input0, input1) = unbundle inputs
        (input0Data, input0HasData) = unbundle input0
        (input1Data, input1HasData) = unbundle input1
        (p0, p1) = unbundle pacings

        outputs = bundle (output0, output1)

        tag = genTag (p0 .||. p1)

        in0Tag = tag
        in1Tag = tag
        out0Tag = tag
        out1Tag = delay invalidTag tag

        enIn0 = input0HasData
        enIn1 = input1HasData
        enOut0 = p0
        enOut1 = delay False p1

        outputPhaseTag = delay invalidTag (delay invalidTag tag)
        output0Aktv = delay False (delay False p0)
        output1Aktv = delay False (delay False p1)

        output0 = bundle (output0Data, output0Aktv)
        (_, output0Data) = unbundle out0
        output1 = bundle (output1Data, output1Aktv)
        (_, output1Data) = unbundle (getMatchingTag <$> out1 <*> outputPhaseTag <*> (pure 0))

        input0Win = input0Window enIn0 (bundle (in0Tag, input0Data))
        input1Win = input1Window enIn1 (bundle (in1Tag, input1Data))

        out0 = outputStream0 enOut0 out0Data0 out0Data1 
        out0Data0 = getOffset <$> input0Win <*> out0Tag <*> (pure 2) <*> (pure 10)
        out0Data1 = getOffset <$> out1 <*> out0Tag <*> (pure 3) <*> (pure 100)

        out1 = outputStream1 enOut1 out1Data0 out1Data1 
        out1Data0 = getMatchingTag <$> input0Win <*> out1Tag <*> (pure 0)
        out1Data1 = input1Win

        debugSignals = tag

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



outputStream0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream0 en in0WithTag out1WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + out1
        (tag, in0) = unbundle in0WithTag
        (_, out1) = unbundle out1WithTag


outputStream1 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int) -> Signal dom (Vec 3 (Tag, Int))
outputStream1 en in0WithTag in1WithTag = result
    where
        result = register (repeat (invalidTag, 0)) (mux en next result)
        next = (<<+) <$> result <*> nextValWithTag
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0 + in1
        (tag, in0) = unbundle in0WithTag
        (_, in1) = unbundle in1WithTag






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
