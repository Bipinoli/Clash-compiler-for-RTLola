module Mixed where

import Clash.Prelude
import Clash.Explicit.Testbench

-- System domain -> 100 MHz clock

counter :: (HiddenClockResetEnable dom, KnownNat a) => Unsigned a -> Signal dom (Unsigned a)
counter toCount = register 0 (mux (prevVal .<. pure (toCount - 1)) (prevVal + 1) 0)
  where prevVal = counter toCount

generateClock :: HiddenClockResetEnable dom => Unsigned 64 -> Unsigned 64 -> Signal dom Bool
generateClock systemClockPeriod desiredPeriod = register False (mux (countSignal .==. maxVal) (pure True) (pure False))
  where
    countSignal = counter toCount
    toCount = desiredPeriod `div` systemClockPeriod
    maxVal = pure (toCount - 1)
-- Test:
-- clashi> sampleN @System 20 (generateClock 1 3)


-- getClockPeriod :: forall dom. HiddenClockResetEnable dom => Integer
-- getClockPeriod = snatToInteger (clockPeriod @dom)

-- generateClock2 :: HiddenClockResetEnable dom => Unsigned 64 -> Signal dom Bool
-- generateClock2 desiredFreq = register False (mux (countSignal .==. maxVal) (pure True) (pure False))
--   where
--     countSignal = counter toCount
--     toCount = systemClockFreq `div` desiredFreq
--     systemClockFreq = getClockPeriod clockPeriod dom
--     maxVal = pure (toCount - 1)
    


last4Values :: HiddenClockResetEnable dom => Signal dom (Signed 32) -> Signal dom (Signed 32, Signed 32, Signed 32, Signed 32)
last4Values inputSignal = bundle (curr, past0, past1, past2)
  where 
    curr = inputSignal
    past0 = register 0 curr
    past1 = register 0 past0
    past2 = register 0 past1
-- Test:
-- clashi> sampleN @System 9 (last4Values $ fromList [1,2,3,4,5,6,7,8,9,10])



streamB :: HiddenClockResetEnable dom => Signal dom (Signed 32) -> Signal dom (Signed 32)
streamB a = register 0 a

streamC :: HiddenClockResetEnable dom => Signal dom (Signed 32) -> Signal dom (Signed 32)
streamC b = register 0 (register 0 b)

windowB :: HiddenClockResetEnable dom => Signal dom (Signed 32) -> Signal dom (Signed 32, Signed 32, Signed 32, Signed 32)
windowB = last4Values

streamD :: HiddenClockResetEnable dom => Signal dom (Signed 32, Signed 32, Signed 32, Signed 32) -> Signal dom (Signed 32)
streamD winB = register 0 sumVal
  where 
    sumVal = fmap (\(x1, x2, x3, x4) -> x1 + x2 + x3 + x4) winB



topEntity :: Clock System -> Reset System -> Enable System -> 
  "a_input" ::: Signal System (Signed 32) -> "b_c_d" ::: Signal System (Signed 32, Signed 32, Signed 32)
topEntity clk rst en a = bundle (strmB, strmC, strmD)
  where
    strmB = exposeClockResetEnable (streamB a) clk rst en
    strmC = exposeClockResetEnable (streamC strmB) clk rst en
    strmD = exposeClockResetEnable (streamD . windowB $ strmB) clk rst dEnable
    dEnable = toEnable $ exposeClockResetEnable (generateClock 1 2) clk rst en



testbench :: Signal System Bool
testbench = done 
    where
        clk = tbSystemClockGen (not <$> done)
        rst = systemResetGen
        en = enableGen
        inputs = stimuliGenerator clk rst ((0 :: Signed 32) :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> Nil)
        -- time 0: (a, b, c, d) = (0, 0, 0, 0)
        -- time 1: (a, b, c, d) = (1, 1, 0, 0) 
        -- time 2: (a, b, c, d) = (2, 2, 0, 3) 
        -- time 3: (a, b, c, d) = (3, 3, 1, 3) 
        -- time 4: (a, b, c, d) = (4, 4, 2, 10) 
        -- time 5: (a, b, c, d) = (5, 5, 3, 10) 
        -- time 6: (a, b, c, d) = (6, 6, 4, 18) 
        -- time 7: (a, b, c, d) = (7, 7, 5, 18) 
        -- time 8: (a, b, c, d) = (8, 8, 6, 26) 
        -- time 9: (a, b, c, d) = (9, 9, 7, 26) 
        -- time 10: (a, b, c, d) = (10, 10, 8, 34) 
        expectedOutputs = outputVerifier' clk rst (
                    (0 :: Signed 32, 0 :: Signed 32, 0 :: Signed 32) :> 
                    (0 :: Signed 32, 0 :: Signed 32, 0 :: Signed 32) :> 
                    (1 :: Signed 32, 0 :: Signed 32, 0 :: Signed 32) :> 
                    (2 :: Signed 32, 0 :: Signed 32, 3 :: Signed 32) :> 
                    (3 :: Signed 32, 1 :: Signed 32, 3 :: Signed 32) :> 
                    (4 :: Signed 32, 2 :: Signed 32, 10 :: Signed 32) :> 
                    Nil
                )
        done = expectedOutputs (topEntity clk rst en inputs)


