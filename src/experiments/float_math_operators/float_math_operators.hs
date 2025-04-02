module FloatMathOperators where

import Clash.Prelude


-- Note:
-- Clash doesn't have blackbox netlist implementaion for floating point number and it's functions
-- We should use Fixed point numbers instead to represent such numbers


a :: SFixed 8 8
a = fromRational 2.3

b :: SFixed 8 8
b = fromRational (-3.3)

aa = a + b
bb = a * b
cc = a / b
dd = abs b




-- plus :: HiddenClockResetEnable dom => Signal dom Float -> Signal dom Float -> Signal dom Float
-- plus a b = a + b

-- mult :: HiddenClockResetEnable dom => Signal dom Float -> Signal dom Float -> Signal dom Float
-- mult a b = a * b

-- myDiv :: HiddenClockResetEnable dom => Signal dom Float -> Signal dom Float -> Signal dom Float
-- myDiv a b = (/) <$> a <*> b

-- pow :: HiddenClockResetEnable dom => Signal dom Float -> Signal dom Float -> Signal dom Float
-- pow a b = (**) <$> a <*> b

-- absolute :: HiddenClockResetEnable dom => Signal dom Float -> Signal dom Float
-- absolute = fmap abs

-- squareRoot :: HiddenClockResetEnable dom => Signal dom Float -> Signal dom Float
-- squareRoot = fmap sqrt

-- -- stream = sqrt(abs(a * b - 100) / 2)
-- stream :: HiddenClockResetEnable dom => Signal dom Float -> Signal dom Float -> Signal dom Float
-- -- stream a b = squareRoot (myDiv absPart (pure 2.0)) 
-- stream a b = absPart
--     where 
--         absPart = (absolute (plus (mult a b) (pure (-100.0))))

-- topEntity :: Clock System -> Reset System -> Enable System -> 
--     Signal System Float -> Signal System Float -> Signal System Float
-- topEntity clk rst en a b = exposeClockResetEnable (plus a b) clk rst en

