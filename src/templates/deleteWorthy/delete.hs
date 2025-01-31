module Delete where

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
timer reset = register 0 (mux reset 0 nextTime)
    where 
        nextTime = timer reset + pure deltaTime
        deltaTime = fromInteger systemClockPeriod :: Int


type Data = Int

slowClock :: HiddenClockResetEnable dom => Signal dom Bool
slowClock = clockDivider 5


evaluator :: HiddenClockResetEnable dom => Signal dom (Data, Bool) -> Signal dom (Bool, (Bool, Int))
evaluator input0 = bundle(slowClock, controls)
    where
        controls = hlc slowClock input0


hlc :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Data, Bool) -> Signal dom (Bool, Int)
hlc en input0 = bundle (bEn, bTimer)
    where
        (bEn, bTimer, _) = unbundle (periodicEnable en bPeriodNs)
        bPeriodNs = 100000 -- 10Hz in ns


periodicEnable :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Int -> Signal dom (Bool, Int, Bool)
periodicEnable en period = register initState (mux en nextState curState)
        where
            initState = (False, 0, False)
            nextState = bundle (nextEn, nextTime, nextRst)
            curState = bundle (curEn, nextTime, curRst)

            (curEn, curTime, curRst) = unbundle (periodicEnable en period)
            nextEn = mux (curTime .>=. period) (pure True) (pure False)
            nextRst = mux (en .&&. (curTime .>=. period)) (pure True) (pure False)
            nextTime = timer nextRst


topEntity :: Clock MyDomain -> Reset MyDomain -> Enable MyDomain -> 
    Signal MyDomain (Data, Bool) -> Signal MyDomain (Bool, (Bool, Int))
topEntity clk rst en input0 = exposeClockResetEnable (evaluator input0) clk rst en
