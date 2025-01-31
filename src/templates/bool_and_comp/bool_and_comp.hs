module BoolAndComp where

import Clash.Prelude



-- TODO: continue from here -> change output to outptut value & aktv bool pairs
evaluator :: HiddenClockResetEnable dom => Signal dom ((Bool, Bool), (Bool, Bool), (Int, Bool)) -> Signal dom ((Bool, Int), (Data, Bool, Bool, Bool, Int, Int, Int), (Int, (Data, Bool), (Data, Bool), (Data, Bool)))
evaluator input0 = bundle(hlcClk, controls, outputs)
    where
        outputs = llc controls stage
        controls = hlc hlcClk input0
        hlcClk = slowClock
        stage = delay 0 (hotPotato 5)
