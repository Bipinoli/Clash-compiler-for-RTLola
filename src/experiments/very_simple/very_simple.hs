module VerySimple where

import Clash.Prelude

---------------------------------------------------------------

-- input a : Int
-- output b := a
-- 

---------------------------------------------------------------

-- Evaluation Order
-- a
-- b

-- Memory Window
-- window a = 1
-- window b = 1

-- Pipeline Visualization
-- a | a | a | a | a | a | a | a | a | a
-- -------------------------------------
--   | b | b | b | b | b | b | b | b | b
-- -------------------------------------

-- input0 = a
-- output0 = b

---------------------------------------------------------------


type HasInput0 = (Int, Bool)
type Inputs = HasInput0

type HasOutput0 = (Int, Bool)
type Outputs = HasOutput0

type Pacings = Bool

type Event = (Inputs, Pacings)

nullEvent :: Event
nullEvent = ((0, False), False)


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
                    (True, False) -> if cur /= length buf then qData +>> buf else buf
                    (_, _) -> buf

        nextCursor :: QCursor -> (QInput, QMem) -> QCursor
        nextCursor cur ((push, pop, _), buf) = out
            where 
                out = case (push, pop) of
                    (True, False) -> if cur /= length buf then cur + 1 else cur
                    (False, True) -> if cur /= 0 then cur - 1 else 0
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
                    (True, _) -> cur /= length buf
                    (_, _) -> False

        nextPopValid :: (QInput, QCursor) -> QPop
        nextPopValid ((push, pop, _), cur) = out
            where 
                out = case (push, pop) of
                    (True, True) -> True
                    (False, True) -> cur /= 0
                    (_, _) -> False


---------------------------------------------------------------



hlc :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Bool, Event)
hlc inputs = out
    where 
        out = bundle (newEvent, event)
        newEvent = pacing0
        event = bundle (inputs, pacings)

        pacings = pacing0

        (_, hasInput0) = unbundle inputs

        pacing0 = hasInput0





---------------------------------------------------------------

type Tag = Unsigned 8
-- maxTag must be at least the size of the sliding window to avoid duplicate tags in the window
maxTag = 10 :: Tag
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


llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom ((Bool, Outputs), (Tag, Bool, Bool))
llc event = bundle (bundle (toPop, outputs), debugSignals)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        toPop = pure True

        (inputs, pacings) = unbundle poppedEvent
        input0 = inputs
        (input0Data, input0HasData) = unbundle input0
        p0 = pacings

        outputs = output0

        tag = genTag (p0)

        in0Tag = tag
        out0Tag = delay invalidTag tag

        enIn0 = input0HasData
        enOut0 = delay False p0

        outputPhaseTag = delay invalidTag (delay invalidTag tag)
        output0Aktv = delay False (delay False p0)

        output0 = bundle (output0Data, output0Aktv)
        (_, output0Data) = unbundle out0

        input0Win = input0Window enIn0 (bundle (in0Tag, input0Data))

        out0 = outputStream0 enOut0 out0Data0 
        out0Data0 = input0Win


        debugSignals = bundle (tag, toPop, isValidEvent)

        genTag :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Tag
        genTag en = t
            where 
                t = register 1 (mux en next_t t)
                next_t = mux (t .==. (pure maxTag)) (pure 1) (t + 1)





input0Window :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
input0Window en td = result
    where result = register (invalidTag, 0) (mux en td result)



outputStream0 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Tag, Int) -> Signal dom (Tag, Int)
outputStream0 en in0WithTag = result
    where
        result = register (invalidTag, 0) (mux en nextValWithTag result)
        nextValWithTag = bundle (tag, nextVal)
        nextVal = in0
        (tag, in0) = unbundle in0WithTag






---------------------------------------------------------------

monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Outputs, (Tag, Bool, Bool, QPushValid, QPopValid, Bool, Bool, Bool))
monitor inputs = bundle (outputs, debugSignals)
    where 
        (newEvent, event) = unbundle (hlc inputs)

        (qPushValid, qPopValid, qPopData) = unbundle (queue (bundle (qPush, qPop, qInptData)))
        qPush = newEvent
        qPop = toPop
        qInptData = event

        (llcOutput, llcDebug) = unbundle (llc (bundle (qPopValid, qPopData)))
        (toPop, outputs) = unbundle llcOutput

        (llcTag, llcToPop, llcIsValidEvent) = unbundle llcDebug
        debugSignals = bundle (llcTag, llcToPop, llcIsValidEvent, qPushValid, qPopValid, newEvent, qPush, qPop)


---------------------------------------------------------------

topEntity :: Clock System -> Reset System -> Enable System -> 
    Signal System Inputs -> Signal System (Outputs, (Tag, Bool, Bool, QPushValid, QPopValid, Bool, Bool, Bool))
topEntity clk rst en inputs = exposeClockResetEnable (monitor inputs) clk rst en
