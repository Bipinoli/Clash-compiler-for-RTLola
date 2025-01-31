module PeriodicEnable where

import Clash.Prelude


-- Create a clock domain with 2 microseconds period (500 kHz) to match the testbench
createDomain vSystem{vName="MyDomain", vPeriod=2000} -- period in nanoseconds

systemClockPeriod = snatToInteger $ clockPeriod @MyDomain


generateClock :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
generateClock desiredPeriodInNs = register False (mux (countSignal .<. halfToCount) (pure True) (pure False))
  where
    countSignal = counter toCount
    toCount = desiredPeriodInNs `div` systemClockPeriod
    halfToCount = pure $ toCount `div` 2

    counter :: HiddenClockResetEnable dom => Integer -> Signal dom Integer
    counter toCount = register 0 (mux (prevVal .<. pure (toCount - 1)) (prevVal + 1) 0)
        where prevVal = counter toCount

clockDivider :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
clockDivider factor = generateClock $ systemClockPeriod * factor


timer :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int
timer reset = register deltaTime (mux reset (pure deltaTime) nextTime)
    where 
        nextTime = timer reset + pure deltaTime
        deltaTime = fromInteger systemClockPeriod :: Int

timerTest :: HiddenClockResetEnable dom => Signal dom Int
timerTest = timerVal
    where 
        timerVal = timer reset
        reset = mux (timerVal .>=. thold) (pure True) (pure False)
        thold = 100000
-- Clashi Test: sampleN @System 300 timerTest


type Data = Int

slowClock :: HiddenClockResetEnable dom => Signal dom Bool
slowClock = clockDivider 5


evaluator :: HiddenClockResetEnable dom => Signal dom (Data, Bool) -> Signal dom (Bool, (Bool, Int, Bool, Int))
evaluator input0 = bundle(slowClock, controls)
    where
        controls = hlc slowClock input0


hlc :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Data, Bool) -> Signal dom (Bool, Int, Bool, Int)
hlc en input0 = bundle (bEn, bTimer, bTimerRst, bCurTime)
    where
        (bEn, bTimer, bTimerRst, bCurTime) = unbundle (periodicEnable en bPeriodNs)
        bPeriodNs = 100000 -- 10Hz in ns


-- NOTE:
-- Output periodic enable singal must sustain until next enable input
-- Timer should immediately restart without being sustained
periodicEnable :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom (Bool, Int, Bool, Int)
periodicEnable en period = register initState (mux en nextState curState)
        where
            initState = (False, 0, False, 0)
            nextState = bundle (nextEn, nextTime, reset, curTime)
            curState = bundle (curEn, nextTime, reset, curTime)

            (curEn, curTime, _, _) = unbundle (periodicEnable en period)
            nextEn = mux (nextTime .>=. period) (pure True) (pure False)
            reset = mux (en .&&. (nextTime .>=. period)) (pure True) (pure False)
            nextTime = timer reset


topEntity :: Clock MyDomain -> Reset MyDomain -> Enable MyDomain -> 
    Signal MyDomain (Data, Bool) -> Signal MyDomain (Bool, (Bool, Int, Bool, Int))
topEntity clk rst en input0 = exposeClockResetEnable (evaluator input0) clk rst en
