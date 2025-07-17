module MyModule where

import Clash.Prelude

data State = Ready | Processing | Done deriving (Generic, NFDataX)
type Input = Vec 3 Float
type Output = Float

transitionFx :: State -> Input -> (State, Output)
transitionFx state input = (newState, output)
  where 
    newState = case state of
      Ready -> Processing
      Processing -> Done
      Done -> Ready
    output = foldl (/) ((head input) * (head input)) input

mealyMachine :: HiddenClockResetEnable dom => Signal dom Input -> Signal dom Output
mealyMachine = mealy transitionFx Ready

type IsValid = Bool

machine :: HiddenClockResetEnable dom => Signal dom (IsValid, Input) -> Signal dom (IsValid, Output)
machine hasInput = register (False, 0) hasOutput
  where
    hasOutput = bundle (outputIsValid, output)
    outputIsValid = (all (/= 0) <$> input) .&&. inputIsValid
    output = mux inputIsValid 
                (mealyMachine (reverse <$> input)) 
                (pure 0.0)
    (inputIsValid, input) = unbundle hasInput

-- let input =  fromList [(repeat 0), (repeat 0), (1:>2:>4:>Nil), (repeat 0), (1:>3:>9:>Nil)] :: Signal dom Input
-- let isValid = fromList [False, False, True, False, True] :: Signal dom IsValid
-- let inputSignal = bundle (isValid, input)
-- let outputSignal = sampleN @System 5 (machine inputSignal)
