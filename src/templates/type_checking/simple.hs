module TypeCheckingSimple where

import Clash.Prelude


newtype DataX = DataX Int
newtype NewX = NewX Bool

newtype DataY = DataY Int
newtype NewY = NewY Bool

type InputData = ((DataX, NewX), (DataY, NewY))

newtype PacingA = PacingA Bool
newtype PacingB = PacingB Bool
newtype PacingC = PacingC Bool
newtype PacingD = PacingD Bool

type Pacings = (PacingA, PacingB, PacingC, PacingD)


evaluate :: InputData -> Pacings -> Bool
evaluate inputData pacings = True
    where
        ((dataX, newX), (dataY, newY)) = inputData
        (enA, enB, enC, enD) = pacings
        

-- must give type check error
-- exampleTypeCheck :: DataX -> DataY -> (DataX, DataY)
-- exampleTypeCheck a b = (b, a)



