module Spec where

import Clash.Prelude
import Clash.Annotations.TopEntity

data ValidInt = ValidInt {
    value :: Int,
    valid :: Bool
} deriving (Generic, NFDataX)

newtype Inputs = Inputs {
    input0 :: ValidInt
} deriving (Generic, NFDataX)


machine :: HiddenClockResetEnable dom => Signal dom Inputs  -> Signal dom Int
machine inputs = pure 42

topEntity :: Clock System -> Reset System -> Enable System ->
    Signal System Inputs -> Signal System Int
topEntity clk rst en inputs = exposeClockResetEnable (machine inputs) clk rst en
{-# ANN topEntity
  (Synthesize
    {
        t_name = "myModule",
        t_inputs = [
            PortName "clk",
            PortName "rst",
            PortName "en",
            PortProduct "input0" [
                PortName "value",
                PortName "valid"
            ]
        ],
        t_output = PortName "out"
    }) #-}
