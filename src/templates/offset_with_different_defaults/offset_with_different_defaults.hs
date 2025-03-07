{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module OffsetWithDiffDefaults where

import Clash.Prelude

---------------------------------------------------------------

-- specification:

-- input a : Int8
-- output b := a.offset(by: -1).defaults(to: 10) -- @a
-- output c := a.offset(by: -1).defaults(to: 100) -- @a

type Data = Signed 8
type HasData = (Data, Bool)
type Inputs = HasData

type Outputs = (HasData, HasData)

class Pacing a where
    unwrap :: a -> Bool

newtype PacingA = PacingA Bool deriving (Generic, NFDataX)
instance Pacing PacingA where
    unwrap (PacingA x) = x

dfltData = 0 :: Data

---------------------------------------------------------------
-- Total odering of events maintained in the queue

type Pacings = PacingA
type Event = (Inputs, Pacings)
-- existing architecture keeps event-based and periodic events separately -> alernate one after another

nullEvent :: Event
nullEvent = ((0, False), PacingA False)
type QMemSize = 6

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

createDomain vSystem{vName="MyDomain", vPeriod=2000} -- period in nanoseconds

hlc :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom (Bool, Event)
hlc inputs = out
    where 
        out = bundle (newEvent, event)

        newEvent = newInput .||. newPacing
        newInput = newA
        newPacing = unwrap <$> pacingA

        event = bundle (inputs, pacings)
        pacings = pacingA

        (_, newA) = unbundle inputs
        pacingA = PacingA <$> newA


---------------------------------------------------------------

data State = StatePop | StateRead | StateEvalLayer0 | StateEvalLayer1 | StateOutput
    deriving (Generic, Eq, NFDataX)

llc :: HiddenClockResetEnable dom => Signal dom (Bool, Event) -> Signal dom (Bool, Outputs)
llc event = bundle (toPop, outputs)
    where 
        state = register StatePop nextStateSignal

        -- state: pop
        toPop = state .==. (pure StatePop)
        (isValidEvent, poppedEvent) = unbundle event
        eventInfo = register nullEvent (mux isValidEvent poppedEvent eventInfo)
        (hasData, pacings) = unbundle eventInfo
        pacingA = pacings
        (a, newA) = unbundle hasData

        -- state: layer 0
        evalA = streamA (state .==. (pure StateEvalLayer0)) a

        -- state: layer 1
        evalB = streamB (state .==. (pure StateEvalLayer1) .&&. (unwrap <$> pacingA)) evalA
        evalC = streamC (state .==. (pure StateEvalLayer1) .&&. (unwrap <$> pacingA)) evalA

        -- state: output
        outAktvB = state .==. (pure StateOutput) .&&. (unwrap <$> pacingA)
        outAktvC = state .==. (pure StateOutput) .&&. (unwrap <$> pacingA)

        outputs = bundle (outputB, outputC)
        outputB = bundle (evalB, outAktvB)
        outputC = bundle (evalC, outAktvC)

        -- state transition
        nextStateSignal = nextState <$> state <*> isValidEvent
        
        nextState :: State -> Bool -> State
        nextState curState validEvent = case curState of
            StatePop -> StateRead
            StateRead -> if validEvent then StateEvalLayer0 else StatePop 
            StateEvalLayer0 -> StateEvalLayer1
            StateEvalLayer1 -> StateOutput
            StateOutput -> StatePop


streamA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom (Vec 2 Data, Vec 2 Bool)
streamA en a = bundle (dataOut, validOut)
    where 
        dataOut = register (repeat dfltData) (mux en ((<<+) <$> dataOut <*> a) dataOut)
        validOut = register (repeat False) (mux en ((<<+) <$> validOut <*> pure True) validOut)


streamB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Vec 2 Data, Vec 2 Bool) -> Signal dom Data
streamB en winA = out
    where 
        (dataWin, validWin) = unbundle winA
        offsetIndx = 0
        offsetData = (!!) <$> dataWin <*> pure offsetIndx
        offsetValid = (!!) <$> validWin <*> pure offsetIndx

        defaultData = 10 :: Data

        out = mux offsetValid offsetData (pure defaultData) 

streamC :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Vec 2 Data, Vec 2 Bool) -> Signal dom Data
streamC en winA = out
    where 
        (dataWin, validWin) = unbundle winA
        offsetIndx = 0
        offsetData = (!!) <$> dataWin <*> pure offsetIndx
        offsetValid = (!!) <$> validWin <*> pure offsetIndx

        defaultData = 100 :: Data

        out = mux offsetValid offsetData (pure defaultData) 


---------------------------------------------------------------


monitor :: HiddenClockResetEnable dom => Signal dom Inputs -> Signal dom Outputs
monitor inputs = outputs
    where 
        (qPushValid, qPopValid, qPopData) = unbundle (queue (bundle (qPush, qPop, qInptData)))

        (newEvent, event) = unbundle (hlc inputs)
        qPush = newEvent
        qInptData = event

        (toPop, outputs) = unbundle (llc (bundle (qPopValid, qPopData)))
        qPop = toPop

---------------------------------------------------------------


topEntity :: Clock MyDomain -> Reset MyDomain -> Enable MyDomain -> 
    Signal MyDomain Inputs -> Signal MyDomain Outputs 
topEntity clk rst en input0 = exposeClockResetEnable (monitor input0) clk rst en

