module PeriodicEnable where

import Clash.Prelude


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

timerTest :: HiddenClockResetEnable dom => Signal dom Int
timerTest = timerVal
    where 
        timerVal = timer reset
        reset = mux (timerVal .>=. thold) (pure True) (pure False)
        thold = 100000
-- Clashi Test: sampleN @System 300 timerTest


type Data = Int

slowClock :: HiddenClockResetEnable dom => Signal dom (Bool, Int)
slowClock = clockDivider 13


evaluator :: HiddenClockResetEnable dom => Signal dom (Data, Bool) -> Signal dom ((Bool, Int), (Bool, Int, Bool, Int, Bool))
evaluator input0 = bundle(hlcClk, controls)
    where
        controls = hlc hlcClk input0
        hlcClk = slowClock


hlc :: HiddenClockResetEnable dom => Signal dom (Bool, Int) -> Signal dom (Data, Bool) -> Signal dom (Bool, Int, Bool, Int, Bool)
hlc hlcClk input0 = bundle (bEn, bTimer, bTimerRst, bCurTime, curEn)
    where
        (bEn, bTimer, bTimerRst, bCurTime, curEn) = unbundle (periodicEnable hlcClk bPeriodNs)
        bPeriodNs = 100000 -- 10Hz in ns


-- NOTE:
-- Once output enable is high, it must sustain until the next cycle
-- Timer should immediately restart without being sustained
periodicEnable :: HiddenClockResetEnable dom => Signal dom (Bool, Int) -> Signal dom Int -> Signal dom (Bool, Int, Bool, Int, Bool)
periodicEnable hlcClk period = register initState (mux (en .&&. (cnt .==. pure 0)) nextState curState) -- cnt .==.0 to sustain for a cycle
        where
            (en, cnt) = unbundle hlcClk 
            initState = (False, 0, False, 0, False)
            nextState = bundle (nextEn, nextTime, reset, curTime, curEn)
            curState = bundle (curEn, nextTime, reset, curTime, curEn)

            (curEn, curTime, _, _, _) = unbundle (periodicEnable hlcClk period)
            nextEn = mux (nextTime .>=. period) (pure True) (pure False)
            reset = mux (en .&&. (nextTime .>=. period)) (pure True) (pure False)
            nextTime = timer reset


topEntity :: Clock MyDomain -> Reset MyDomain -> Enable MyDomain -> 
    Signal MyDomain (Data, Bool) -> Signal MyDomain ((Bool, Int), (Bool, Int, Bool, Int, Bool))
topEntity clk rst en input0 = exposeClockResetEnable (evaluator input0) clk rst en
