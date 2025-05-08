module TupleBug where

import Clash.Prelude

circuit :: HiddenClockResetEnable dom => Signal dom (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
circuit = outs
    where
        outs = bundle (out0, out1, out2, out3, out4, out5, out6, out7, out8, out9, out10, out11, out12)
        out0 = register 0 out0
        out1 = register 0 out1
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

topEntity :: Clock System -> Reset System -> Enable System -> Signal System (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
topEntity clk rst en = exposeClockResetEnable circuit clk rst en