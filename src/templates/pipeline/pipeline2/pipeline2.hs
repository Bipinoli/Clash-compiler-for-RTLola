module Pipeline2 where

import Clash.Prelude


---------------------------------------------------------------

type DataX = Int

type HasDataX = (DataX, Bool)
type Inputs = HasDataX
type Outputs = (HasDataX, HasDataX, HasDataX, HasDataX)


---------------------------------------------------------------

type Pacings = Bool
type Event = (Inputs, Pacings)

nullEvent :: Event
nullEvent = ((0, False), False)
type QMemSize = 20

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

        -- Signal is an applicative functor
        -- <$> = alias for fmap - wraps normal function into a function on Signal
        -- <*> = apply applicative - it applies signal of function to a signal of a value

        nextBufferSignal = nextBuffer <$> buffer <*> bundle (input, cursor)
        nextCursorSignal = nextCursor <$> cursor <*> bundle (input, buffer)
        nextOutDataSignal = nextOutData <$> bundle (input, cursor, buffer)
        nextPushValidSignal = nextPushValid <$> bundle (input, cursor, buffer)
        nextPopValidSignal = nextPopValid <$> bundle (input, cursor)
        
        nextBuffer :: QMem -> (QInput, QCursor) -> QMem
        nextBuffer buf ((push, pop, qData), cur) = out
            where 
                out = case (push, pop) of
                    (True, _) -> if cur /= length buf then qData +>> buf else buf
                    (_, _) -> buf

        nextCursor :: QCursor -> (QInput, QMem) -> QCursor
        nextCursor cur ((push, pop, _), buf) = out
            where 
                out = case (push, pop) of
                    (True, False) -> if cur /= length buf then cur + 1 else cur
                    (False, True) -> if cur /= 0 then cur - 1 else 0
                    (True, True) -> if cur == 0 then cur + 1 else cur 
                    (_, _) -> cur

        nextOutData :: (QInput, QCursor, QMem) -> QData
        nextOutData ((push, pop, _), cur, buf) = out
            where 
                out = case (push, pop) of
                    (_, True) -> if cur == 0 then nullEvent else buf !! (cur - 1)
                    (_, _) -> nullEvent

        nextPushValid :: (QInput, QCursor, QMem) -> QPush
        nextPushValid ((push, pop, _), cur, buf) = out
            where 
                out = case (push, pop) of
                    (True, _) -> cur /= length buf
                    (_, _) -> False

        nextPopValid :: (QInput, QCursor) -> QPop
        nextPopValid ((push, pop, _), cur) = out
            where 
                out = case (push, pop) of
                    (_, True) -> cur /= 0
                    (_, _) -> False

---------------------------------------------------------------


monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom Outputs
monitor inputs = outputs
    where 
        (newEvent, event) = unbundle (hlc inputs)

        (qPushValid, qPopValid, qPopData) = unbundle (queue (bundle (qPush, qPop, qInptData)))
        qPush = newEvent
        qInptData = event

        (toPop, outputs) = unbundle (llc (bundle (qPopValid, qPopData)))
        qPop = toPop


hlc :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Bool, Event)
hlc inputs = out
    where 
        out = bundle (newEvent, event)
        newEvent = newX
        (_, newX) = unbundle inputs
        event = bundle (inputs, pacings)
        pacings = newX
 

llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom (Bool, Outputs)
llc event = bundle (toPop, outputs)
    where 
        toPop = pure True 
        (isValidEvent, poppedEvent) = unbundle event
        (inputs, pacings) = unbundle poppedEvent

        (x, _) = unbundle inputs

        outputs = bundle (outputA, outputB, outputC, outputD)

        outputA = bundle (outA, aktvOutA)
        outputB = bundle (head <$> outB, aktvOutB)
        outputC = bundle (outC, aktvOutC)
        outputD = bundle (outD, aktvOutD)

        pacingA = pacings
        pacingB = pacings
        pacingC = delay False pacingA
        pacingD = delay False pacingC

        outA = streamA pacingA x
        outB = streamB pacingB x
        outC = streamC pacingC outA
        outD = streamD pacingD (last <$> outB) outC

        aktvOutA = delay False pacings
        aktvOutB = delay False pacings
        aktvOutC = delay False aktvOutA
        aktvOutD = delay False aktvOutC


streamA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom DataX
streamA en x = out
    where out = register 0 (mux en (x + 1) out)

streamB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom (Vec 3 DataX)
streamB en a = out
    where out = register (repeat 0) (mux en ((+>>) <$> (a + 1) <*> out) out)

streamC :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom DataX
streamC en a = out
    where out = register 0 (mux en (a + 1) out)

streamD :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom DataX -> Signal dom DataX
streamD en c b = out
    where out = register 0 (mux en (c + b) out)


---------------------------------------------------------------

topEntity :: Clock System -> Reset System -> Enable System -> 
    Signal System Inputs -> Signal System Outputs
topEntity clk rst en input0 = exposeClockResetEnable (monitor input0) clk rst en