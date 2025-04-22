module Pipeline1 where

import Clash.Prelude


---------------------------------------------------------------

-- newtype DataX = DataX Int deriving (Generic, NFDataX, Show)

-- unwrapDataX :: DataX -> Int
-- unwrapDataX (DataX x) = x

type DataX = Int

type HasDataX = (DataX, Bool)
type Inputs = HasDataX
type Outputs = (HasDataX, HasDataX, HasDataX, HasDataX, HasDataX, HasDataX, HasDataX)


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


monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Outputs, (Bool, Bool))
monitor inputs = bundle (outputs, llcDebug)
    where 
        (newEvent, event) = unbundle (hlc inputs)

        (qPushValid, qPopValid, qPopData) = unbundle (queue (bundle (qPush, qPop, qInptData)))
        qPush = newEvent
        qInptData = event

        (llcOutput, llcDebug) = unbundle (llc (bundle (qPopValid, qPopData)))
        (toPop, outputs) = unbundle llcOutput
        qPop = toPop


hlc :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Bool, Event)
hlc inputs = out
    where 
        out = bundle (newEvent, event)
        newEvent = newX
        (_, newX) = unbundle inputs
        event = bundle (inputs, pacings)
        pacings = newX
 

llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom ((Bool, Outputs), (Bool, Bool))
llc event = bundle (bundle (toPop, outputs), debugSignals)
    where 
        (isValidEvent, poppedEvent) = unbundle event

        isPipelineReady = pipelineReady startNewPipeline 
        startNewPipeline = mux (isPipelineReady .&&. isValidEvent) (pure True) (pure False)
        toPop = isPipelineReady .&&. not <$> startNewPipeline

        (inputs, pacings) = unbundle poppedEvent
        (x, _) = unbundle inputs

        outputs = bundle (outputA, outputB, outputC, outputD, outputE, outputF, outputG)

        outputA = bundle (outA, aktvOutA)
        outputB = bundle (outB, aktvOutB)
        outputC = bundle (outC, aktvOutC)
        outputD = bundle (outD, aktvOutD)
        outputE = bundle (outE, aktvOutE)
        outputF = bundle (outF, aktvOutF)
        outputG = bundle (outG, aktvOutG)

        pacingA = pacings
        pacingB = delay False pacingA
        pacingC = delay False pacingB
        pacingD = pacingC
        pacingE = delay False pacingD
        pacingF = delay False pacingE
        pacingG = delay False pacingF

        outA = streamA pacingA x outC
        outB = streamB pacingB outA
        outC = streamC pacingC outB
        outD = streamD pacingD outB
        outE = streamE pacingE outD
        outF = streamF pacingF outE
        outG = streamG pacingG outF

        aktvOutA = delay False pacings
        aktvOutB = delay False aktvOutA
        aktvOutC = delay False aktvOutB
        aktvOutD = aktvOutC
        aktvOutE = delay False aktvOutD
        aktvOutF = delay False aktvOutE
        aktvOutG = delay False aktvOutF

        debugSignals = bundle (isPipelineReady, startNewPipeline)


pipelineReady :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool
pipelineReady rst = toWait .==. pure 0 
    where 
        waitTime = pure 2 :: Signal dom Int
        toWait = register (0 :: Int) next
        next = mux rst waitTime (mux (toWait .>. pure 0) (toWait - 1) toWait)


streamA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom DataX -> Signal dom DataX
streamA en x c = out
    where out = register 0 (mux en (x + c) out)

streamB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom DataX
streamB en a = out
    where out = register 0 (mux en (a + 1) out)

streamC :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom DataX
streamC en b = out
    where out = register 0 (mux en (b + 1) out)

streamD :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom DataX
streamD en b = out
    where out = register 0 (mux en (b + 1) out)

streamE :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom DataX
streamE en d = out
    where out = register 0 (mux en (d + 1) out)

streamF :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom DataX
streamF en e = out
    where out = register 0 (mux en (e + 1) out)

streamG :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataX -> Signal dom DataX
streamG en f = out
    where out = register 0 (mux en (f + 1) out)




---------------------------------------------------------------

topEntity :: Clock System -> Reset System -> Enable System -> 
    Signal System Inputs -> Signal System (Outputs, (Bool, Bool))
topEntity clk rst en input0 = exposeClockResetEnable (monitor input0) clk rst en