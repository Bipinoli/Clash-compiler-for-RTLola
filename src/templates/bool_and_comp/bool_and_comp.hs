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

type DataAB = Bool
type DataId = Signed 8

---------------------------------------------------------------


evaluator :: HiddenClockResetEnable dom => 
    Signal dom ((DataAB, Bool), (DataAB, Bool), (DataId, Bool)) -> 
    Signal dom (
        (Bool, Int),
        (
            DataAB, DataAB, DataId, 
            Bool, Bool, Bool, Bool, Bool, Bool
        ),
        (
            Int,
            (DataAB, Bool), 
            (DataAB, Bool),
            (DataAB, Bool),
            (DataAB, Bool),
            (DataAB, Bool),
            (DataId, Bool)
        )
    )
evaluator input0 = bundle(hlcClk, controls, outputs)
    where
        outputs = llc controls stage
        controls = hlc hlcClk input0
        hlcClk = slowClock
        stage = delay 0 (hotPotato 5)


---------------------------------------------------------------

hlc :: HiddenClockResetEnable dom => 
    Signal dom (Bool, Int) -> 
    Signal dom ((DataAB, Bool), (DataAB, Bool), (DataId, Bool)) ->
    Signal dom (DataAB, DataAB, DataId, 
                Bool, Bool, Bool, Bool, Bool, Bool)
hlc hlcClk input0 = register (False, False, 0, False, False, False, False, False, False) (mux en newResult oldResult)
    where
        oldResult = hlc hlcClk input0
        (en, _) = unbundle hlcClk
        newResult = bundle (a, b, id, enLt, enGt, enNeq, enNotA, enAImplB, enTimeStream)
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
    Signal dom (
        DataAB, DataAB, DataId, 
        Bool, Bool, Bool, Bool, Bool, Bool
    ) -> 
    Signal dom Int -> 
    Signal dom (
        Int,
        (DataAB, Bool), 
        (DataAB, Bool),
        (DataAB, Bool),
        (DataAB, Bool),
        (DataAB, Bool),
        (DataId, Bool)
    )
llc controls stage = bundle (stage, resultLt, resultGt, resultNeq, resultNotA, resultAImplB, resultTimeStream)
    where 
        resultLt = bundle (outLt, aktvLt)
        resultGt = bundle (outGt, aktvGt)
        resultNeq = bundle (outNeq, aktvNeq)
        resultNotA = bundle (outNotA, aktvNotA)
        resultAImplB = bundle (outAImplB, aktvAImplB)
        resultTimeStream = bundle (outTimeStream, aktvTimeStream)
        -- stage 0 & stage 1: HLC RTL tranfers (2 cycles)
        -- stage 2 : layer 1 -> aImplB, notA, timeStream, gt, lt
        outAImplB = evaluateAImplB ((stage .==. pure 2) .&&. enAImplB) (bundle (a, b))
        outNotA = evaluateNotA ((stage .==. pure 2) .&&. enNotA) a
        outTimeStream = evaluateTimeStream ((stage .==. pure 2) .&&. enTimeStream) id
        outGt = evaluateGt ((stage .==. pure 2) .&&. enGt) id
        outLt = evaluateLt ((stage .==. pure 2) .&&. enLt) id
        -- stage 3: layer 2 -> neq
        outNeq = evaluateNeq ((stage .==. pure 3) .&&. enNeq) (bundle (outLt, outGt))
        -- stage 4: output
        (aktvLt, aktvGt, aktvNeq, aktvNotA, aktvAImplB, aktvTimeStream) = unbundle $ mux (stage .==. pure 4) 
            (bundle (enLt, enGt, enNeq, enNotA, enAImplB, enTimeStream)) 
            (bundle (pure False, pure False, pure False, pure False, pure False, pure False))  
        (a, b, id, enLt, enGt, enNeq, enNotA, enAImplB, enTimeStream) = unbundle controls


evaluateAImplB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (DataAB, DataAB) -> Signal dom DataAB
evaluateAImplB en inpt = register False (mux en (fmap not a .||. b) oldVal)
    where 
        oldVal = evaluateAImplB en inpt
        (a, b) = unbundle inpt

evaluateNotA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataAB -> Signal dom DataAB
evaluateNotA en inpt = register False (mux en (fmap not a) oldVal)
    where 
        oldVal = evaluateNotA en inpt
        a = inpt

evaluateTimeStream :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataId -> Signal dom DataId
evaluateTimeStream en inpt = register 0 (mux en id oldVal)
    where 
        oldVal = evaluateTimeStream en inpt
        id = inpt

evaluateGt :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataId -> Signal dom DataAB
evaluateGt en inpt = register False (mux en (id .>. pure 5) oldVal)
    where 
        oldVal = evaluateGt en inpt
        id = inpt

evaluateLt :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom DataId -> Signal dom DataAB
evaluateLt en inpt = register False (mux en (id .<. pure 5) oldVal)
    where 
        oldVal = evaluateLt en inpt
        id = inpt

evaluateNeq :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (DataAB, DataAB) -> Signal dom DataAB
evaluateNeq en inpt = register False (mux en (lt .||. gt) oldVal)
    where 
        oldVal = evaluateNeq en inpt
        (lt, gt) = unbundle inpt


---------------------------------------------------------------


topEntity :: Clock MyDomain -> Reset MyDomain -> Enable MyDomain -> 
    Signal MyDomain ((DataAB, Bool), (DataAB, Bool), (DataId, Bool)) -> 
    Signal MyDomain (
        (Bool, Int),
        (
            DataAB, DataAB, DataId, 
            Bool, Bool, Bool, Bool, Bool, Bool
        ),
        (
            Int,
            (DataAB, Bool), 
            (DataAB, Bool),
            (DataAB, Bool),
            (DataAB, Bool),
            (DataAB, Bool),
            (DataId, Bool)
        )
    )
topEntity clk rst en input0 = exposeClockResetEnable (evaluator input0) clk rst en
