module Rough where

import Clash.Prelude

type State = Vec 6 Int
type Input = Int
type Output = State

fx :: State -> Input -> (State, Output)
fx state input = (nextState, nextState)
    where 
        nextState = input +>> state

machine :: HiddenClockResetEnable dom => Signal dom Input -> Signal dom Output
machine = mealy fx (repeat 0)

topEntity :: Clock System -> Reset System -> Enable System 
    -> Signal System Input -> Signal System Output
topEntity = exposeClockResetEnable machine


--- TEST --
inputs :: HiddenClockResetEnable dom => Signal dom Input
inputs = fromList [0,0,1,2,3,4,5,6,7,8]

outputs = sampleN @System 10 (machine inputs)


