module Mixed2 where

import Clash.Prelude
import Clash.Explicit.Testbench

-- System domain -> 100 MHz clock -> 10_000 ps
systemClockPeriod = snatToInteger $ clockPeriod @System

generateClock :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
generateClock desiredPeriod = register False (mux (countSignal .==. maxVal) (pure True) (pure False))
  where
    countSignal = counter toCount
    toCount = desiredPeriod `div` systemClockPeriod
    maxVal = pure (toCount - 1)

    counter :: HiddenClockResetEnable dom => Integer -> Signal dom Integer 
    counter toCount = register 0 (mux (prevVal .<. pure (toCount - 1)) (prevVal + 1) 0)
        where prevVal = counter toCount
-- Test: (generating the clock which is half as fast as the system clk)
-- clashi> sampleN @System 10 $ generateClock 20000 

clockReducer :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
clockReducer factor = generateClock $ systemClockPeriod * factor


-------------- Design notes ---------
-- it takes 1 cycle for register transfer 
-- let's keep a ghost register where the value will be inputted without caring for it's pacing
-- and as per the pacing the enable signal will be generated to pull the value from the ghost

-- on a right time emit enable signals to schedule the streams evaluation

-- enable signals are not appropriate to use as clock as they are not edge triggered

inputCheckSignal :: HiddenClockResetEnable dom => Signal dom Bool
inputCheckSignal = not <$> clockReducer 2

evaluationSignal :: HiddenClockResetEnable dom => Signal dom Bool
evaluationSignal = clockReducer 2
-- Test:
-- clashi> sampleN @System 10 evaluationSignal


bEnable :: HiddenClockResetEnable dom => Signal dom Bool
bEnable = generateClock 40_000 -- 25 MHz -> 40_000 ps period

dEnable :: HiddenClockResetEnable dom => Signal dom Bool
dEnable = generateClock 80_000 -- 12.5 MHz -> 80_000 ps period


-- -- TODO: use the existing enable signal
streamB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom (Signed 32) -> Signal dom (Signed 32)
streamB en a = register 0 (mux en a oldVal)
    where oldVal = streamB en a
-- Test
-- clashi> sampleN @System 11 $ streamB (fromList [True, True, True, True, True, False, False, False, True
-- , True, False]) (fromList [1,2,3,4,5,6,7,8,9,10,11])


topEntity :: Clock System -> Reset System -> Enable System -> 
    Signal System (Signed 32) -> Signal System (Signed 32, Signed 32)
topEntity clk rst en a = bundle (a, b)
    where 
        b = exposeClockResetEnable (streamB enbl a) clk rst en
        enbl = exposeClockResetEnable bEnable clk rst en

-- topEntity :: Clock System -> Reset System -> Enable System -> 
--     Signal System (Signed 32) -> Signal System (Signed 32, Signed 32, Signed 32)
-- topEntity clk rst en a = bundle (a, bG, b)
--     where 
--         b = exposeClockResetEnable (streamB bEnable bG) clk rst en
--         bG = exposeClockResetEnable (bGhost a) clk rst en



-- simulation ::  Signal System (Signed 32, Signed 32, Signed 32)
-- simulation = result 
--     where
--         clk = tbSystemClockGen (not <$> done)
--         rst = systemResetGen
--         en = enableGen
--         a = stimuliGenerator clk rst ((0 :: Signed 32) :> 0 :> 1 :> 1 :> 2 :> 2 :> 3 :> 3 :> 4 :> 4 :> Nil) 
--         result = topEntity clk rst en a
--         done = pure False
-- -- Test:
-- -- clashi> sampleN @System 10 simulation


-- -- testbench :: Signal System Bool
-- -- testbench = done 
-- --     where
-- --         clk = tbSystemClockGen (not <$> done)
-- --         rst = systemResetGen
-- --         en = enableGen
-- --         inputs = stimuliGenerator clk rst ((0 :: Signed 32) :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> Nil)
-- --         -- time 0: (a, b, c, d) = (0, 0, 0, 0)
-- --         -- time 1: (a, b, c, d) = (1, 1, 0, 0) 
-- --         -- time 2: (a, b, c, d) = (2, 2, 0, 3) 
-- --         -- time 3: (a, b, c, d) = (3, 3, 1, 3) 
-- --         -- time 4: (a, b, c, d) = (4, 4, 2, 10) 
-- --         -- time 5: (a, b, c, d) = (5, 5, 3, 10) 
-- --         -- time 6: (a, b, c, d) = (6, 6, 4, 18) 
-- --         -- time 7: (a, b, c, d) = (7, 7, 5, 18) 
-- --         -- time 8: (a, b, c, d) = (8, 8, 6, 26) 
-- --         -- time 9: (a, b, c, d) = (9, 9, 7, 26) 
-- --         -- time 10: (a, b, c, d) = (10, 10, 8, 34) 
-- --         expectedOutputs = outputVerifier' clk rst (
-- --                     (0 :: Signed 32) :> 
-- --                     (2) :>
-- --                     (4) :> 
-- --                     Nil
-- --                 )
-- --         done = expectedOutputs (topEntity clk rst en inputs)
