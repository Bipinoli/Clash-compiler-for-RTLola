module BoolAndComp where

import Clash.Prelude

---------------------------------------------------------------

-- Create a clock domain with 2 microseconds period (500 kHz) to match the testbench
createDomain vSystem{vName="MyDomain", vPeriod=2000} -- period in nanoseconds

systemClockPeriod :: Int
systemClockPeriod = fromInteger (snatToInteger $ clockPeriod @MyDomain)


generateClock :: HiddenClockResetEnable dom => Int -> Signal dom (Bool, Int)
generateClock desiredPeriodInNs = register initClk (mux (countSignal .<. halfToCount) hiClk loClk)
  where
    initClk = (False, 0)
    hiClk = bundle (pure True, countSignal)
    loClk = bundle (pure False, countSignal)

    countSignal = counter toCount
    toCount = desiredPeriodInNs `div` systemClockPeriod
    halfToCount = pure $ toCount `div` 2

    counter :: HiddenClockResetEnable dom => Int -> Signal dom Int
    counter toCount = register 0 (mux (prevVal .<. pure (toCount - 1)) (prevVal + 1) 0)
        where prevVal = counter toCount

clockDivider :: HiddenClockResetEnable dom => Int -> Signal dom (Bool, Int)
clockDivider factor = generateClock $ systemClockPeriod * factor


timer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int
timer reset = register deltaTime (mux reset (pure deltaTime) nextTime)
    where 
        nextTime = timer reset + pure deltaTime
        deltaTime = systemClockPeriod


hotPotato :: HiddenClockResetEnable dom => Int -> Signal dom Int
hotPotato n = let s = register 0 (mux (s .==. pure (n-1)) 0 (s + 1)) in s

slowClock :: HiddenClockResetEnable dom => Signal dom (Bool, Int)
slowClock = clockDivider 5


---------------------------------------------------------------


evaluator :: HiddenClockResetEnable dom => Signal dom ((Bool, Bool), (Bool, Bool), (Int, Bool)) -> 
    Signal dom ((Bool, Bool), 
                (Bool, Bool),
                (Bool, Bool),
                (Bool, Bool),
                (Bool, Bool),
                (Int, Bool)
                )
evaluator input0 = bundle(hlcClk, controls, outputs)
    where
        outputs = llc controls stage
        controls = hlc hlcClk input0
        hlcClk = slowClock
        stage = delay 0 (hotPotato 5)


---------------------------------------------------------------

hlc :: HiddenClockResetEnable dom => 
    Signal dom Bool -> 
    Signal dom ((Bool, Bool), (Bool, Bool), (Int, Bool)) ->
    Signal dom (Bool, Bool, Int, 
                Bool, Bool, Bool, Bool, Bool, Bool)
hlc hlcClk input0 = register (False, False, 0, False, False, False, False, False, False) (mux en newResult oldResult)
    where
        oldResult = hlc en input0
        newResult = bundle (a, b, id, enLt, enGt, enNeq,  enA, enB, enC)
        (aa, bb, idd) = unbundle input0
        (a, aktvA) = unbundle aa
        (b, aktvB) = unbundle bb
        (id, aktvId) = unbundle idd

        enLt = aktvId
        enGt = aktvId
        enNeq = aktvId

        enNotA = aktvA
        enAImplB = aktvA .&&. aktvB

        -- periodic enables bring one extra clock-cycle for RTL transfer in HLC
        (enTimeStream, _) = unbundle (periodicEnable hlcClk timeStreamPeriodNs) 
        timeStreamPeriodNs = 1000000000 -- 1kHz in ns


periodicEnable :: HiddenClockResetEnable dom => Signal dom (Bool, Int) -> Signal dom Int -> Signal dom (Bool, Int)
periodicEnable hlcClk period = register initState (mux (en .&&. (cnt .==. pure 0)) nextState curState) -- cnt .==.0 to sustain for a cycle
        where
            (en, cnt) = unbundle hlcClk 
            initState = (False, 0)
            nextState = bundle (nextEn, nextTime)
            curState = bundle (curEn, nextTime)

            (curEn, curTime) = unbundle (periodicEnable hlcClk period)
            nextEn = mux (nextTime .>=. period) (pure True) (pure False)
            reset = mux (en .&&. (nextTime .>=. period)) (pure True) (pure False)
            nextTime = timer reset


---------------------------------------------------------------

llc :: HiddenClockResetEnable dom => 
    Signal dom (Bool, Bool, Int, 
                Bool, Bool, Bool, Bool, Bool, Bool) -> 
    Signal dom Int -> 
    Signal dom ((Bool, Bool), 
                (Bool, Bool),
                (Bool, Bool),
                (Bool, Bool),
                (Bool, Bool),
                (Int, Bool)
                )
llc controls stage = bundle (stage, resultB, resultC, resultD)
    where 
        resultB = bundle (outB, aktvB)
        resultC = bundle (outC, aktvC)
        resultD = bundle (outD, aktvD)
        -- stage 0 & stage 1: HLC RTL tranfers (2 cycles)
        -- stage 2 : layer 1 -> aImplB, notA, timeStream, gt, lt

-- TODO: continue from here -> 
        outAImplB = evaluateAImplB (())   

        outB = evaluateB ((stage .==. pure 2) .&&. enB) a
        outC = evaluateC ((stage .==. pure 2) .&&. enC) a
        -- stage 3: layer 2 -> neq
        outD = evaluateD ((stage .==. pure 3) .&&. enD) (bundle (outB, outC))
        -- stage 4: output
        (aktvB, aktvC, aktvD) = unbundle $ mux (stage .==. pure 4) (bundle (enB, enC, enD)) (bundle (pure False, pure False, pure False)) 
        (a, enB, enC, enD, _, _, _) = unbundle controls


evaluateB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data
evaluateB en inpt = register 10 (mux en a oldVal)
    where 
        oldVal = evaluateB en inpt
        a = inpt

evaluateC :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data
evaluateC en inpt = register 10 (mux en a oldVal)
    where 
        oldVal = evaluateB en inpt
        a = inpt

evaluateD :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Data, Data) -> Signal dom Data
evaluateD en inpt = register 10 (mux en (b + c) oldVal)
    where 
        oldVal = evaluateD en inpt
        (b, c) = unbundle inpt


