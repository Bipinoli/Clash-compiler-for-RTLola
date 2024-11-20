module DynamicStream where

import Clash.Prelude

systemClockPeriod = snatToInteger $ clockPeriod @System

generateClock :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
generateClock desiredPeriodInPs = register False (mux (countSignal .<. halfToCount) (pure True) (pure False))
  where
    countSignal = counter toCount
    toCount = desiredPeriodInPs `div` systemClockPeriod
    halfToCount = pure $ toCount `div` 2

    counter :: HiddenClockResetEnable dom => Integer -> Signal dom Integer
    counter toCount = register 0 (mux (prevVal .<. pure (toCount - 1)) (prevVal + 1) 0)
        where prevVal = counter toCount

clockDivider :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
clockDivider factor = generateClock $ systemClockPeriod * factor

----------------------------------------

data StreamState = Closed | Spawned
    deriving (Eq, Show)

state2Bit :: StreamState -> Bit
state2Bit Closed = 0
state2Bit Spawned = 1

bit2State :: Bit -> StreamState
bit2State 0 = Closed
bit2State 1 = Spawned

type Data = Signed 32


distanceSpawn :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom Bool
distanceSpawn a b = a .&&. b

distanceEval :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Bool -> Signal dom Bool
distanceEval a b = a .&&. b

distanceFx :: Bit -> (Bool, Bool, Data, Data) -> (Bit, Data)
distanceFx stateBit (spawnCond, evalCond, a, b) = (state2Bit nextState, outputValue)
    where
        nextState = case state of
            Closed -> (if spawnCond then Spawned else Closed)
            Spawned -> Spawned
        outputValue = case state of
            Spawned -> (if evalCond then evaluatedValue else oldValue)
            _ -> oldValue
        state = bit2State stateBit
        evaluatedValue = b - a
        (_, oldValue) = distanceFx stateBit (spawnCond, evalCond, a, b)

distance :: HiddenClockResetEnable dom => Signal dom (Bool, Bool, Data, Data) -> Signal dom Data
distance = mealy distanceFx (state2Bit Closed)
