{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TupleBug where

import Clash.Prelude

data Ckt1Output = Ckt1Output
    { out0  :: Int
    , out1  :: Int
    , out2  :: Int
    , out3  :: Int
    , out4  :: Int
    , out5  :: Int
    , out6  :: Int
    , out7  :: Int
    , out8  :: Int
    , out9  :: Int
    , out10 :: Int
    , out11 :: Int
    , out12 :: Int
    , out13 :: Int
    , out14 :: Int
    , out15 :: Int
    , out16 :: Int
    } deriving (Generic, NFDataX)


data Ckt2Output = Ckt2Output
    { out0 :: Int
    , out1 :: Int
    , out16 :: Int
    } deriving (Generic, NFDataX)


circuit2 :: HiddenClockResetEnable dom => Signal dom Ckt2Output
circuit2 = outs
    where 
        outs = Ckt2Output <$> out0 <*> out1 <*> out16
        out0 = register 0 out0
        out1 = register 0 out1
        out16 = register 0 out16

circuit1 :: HiddenClockResetEnable dom => Signal dom Ckt1Output
circuit1 = outs
    where
        outs = Ckt1Output <$> out0 <*> out1 <*> out2 <*> out3 <*> out4 <*> out5 <*> out6 <*> out7 <*> out8 <*> out9 <*> out10 <*> out11 <*> out12 <*> out13 <*> out14 <*> out15 <*> out16

        ckt2 = circuit2

        out0 = (.out0) <$> ckt2
        out1 = (.out1) <$> ckt2
        out16 = (.out16) <$> ckt2

        out2 = register 0 out2
        out3 = register 0 out3
        out4 = register 0 out4
        out5 = register 0 out5
        out6 = register 0 out6
        out7 = register 0 out7
        out8 = register 0 out8
        out9 = register 0 out9
        out10 = register 0 out10
        out11 = register 0 out11
        out12 = register 0 out12
        out13 = register 0 out13
        out14 = register 0 out14
        out15 = register 0 out15


topEntity :: Clock System -> Reset System -> Enable System -> Signal System Ckt1Output
topEntity clk rst en = exposeClockResetEnable circuit1 clk rst en