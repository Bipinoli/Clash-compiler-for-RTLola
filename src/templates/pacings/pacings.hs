module Pacings where

import Clash.Prelude

systemClockPeriod = snatToInteger $ clockPeriod @System

generateClock :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
generateClock desiredPeriodInPs = register False (mux (countSignal .<. halfToCount) (pure True) (pure False))
  where
    countSignal = counter toCount
    toCount = desiredPeriodInPs `div` systemClockPeriod
    halfToCount = pure $ toCount `div` 2

    counter :: HiddenClockResetEnable dom => Integer -> Signal dom Integer
    counter toCount = register 0 (mux (prevVal .<. pure (toCount - 1)) (prevVal + 1) 0)
        where prevVal = counter toCount

clockDivider :: HiddenClockResetEnable dom => Integer -> Signal dom Bool
clockDivider factor = generateClock $ systemClockPeriod * factor

----------------------------------------
type Data = Signed 32
type HasData = Bool

pacingX1 :: HiddenClockResetEnable dom => Signal dom HasData -> Signal dom HasData -> Signal dom HasData -> Signal dom Bool
pacingX1 x1 x2 x3 = x1

pacingX3 :: HiddenClockResetEnable dom => Signal dom HasData -> Signal dom HasData -> Signal dom HasData -> Signal dom Bool
pacingX3 x1 x2 x3 = x3

pacingX1orX2 :: HiddenClockResetEnable dom => Signal dom HasData -> Signal dom HasData -> Signal dom HasData -> Signal dom Bool
pacingX1orX2 x1 x2 x3 = x1 .||. x2

pacingX1andX2 :: HiddenClockResetEnable dom => Signal dom HasData -> Signal dom HasData -> Signal dom HasData -> Signal dom Bool
pacingX1andX2 x1 x2 x3 = x1 .&&. x2

pacingAny :: HiddenClockResetEnable dom => Signal dom HasData -> Signal dom HasData -> Signal dom HasData -> Signal dom Bool
pacingAny x1 x2 x3 = x1 .||. x2 .||. x3

pacingAll :: HiddenClockResetEnable dom => Signal dom HasData -> Signal dom HasData -> Signal dom HasData -> Signal dom Bool
pacingAll x1 x2 x3 = x1 .&&. x2 .&&. x3

-- X1 or (X1 and x2) is logically equivalent to X1
-- Logical formula in pacing can be simplified to reduce the resource use in underlying hardware
pacingX1orX1andX2 :: HiddenClockResetEnable dom => Signal dom HasData -> Signal dom HasData -> Signal dom HasData -> Signal dom Bool
pacingX1orX1andX2 x1 x2 x3 = x1 .||. (x1 .&&. x2)

pacing10kHz :: HiddenClockResetEnable dom => Signal dom Bool
pacing10kHz = generateClock 100_000_000

pacing5kHz :: HiddenClockResetEnable dom => Signal dom Bool
pacing5kHz = generateClock 200_000_000


----------------------------------------
window3 :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Data -> Signal dom (Data, Data, Data)
window3 enable x deflt = bundle (cur, past1, past2) 
  where 
    cur = register deflt (mux enable x cur)
    past1 = register deflt (mux enable cur past1)
    past2 = register deflt (mux enable past1 past2)
-- clashi Test:
-- let x = fromList [(1 :: Signed 32), 2,3,4,5,6,7,8]
-- let en = fromList [True, True, True, True, True, True, True, True]
-- sampleN @System 8 (window3 en x (-1))
-- Note: in sampleN reset is True for first 2 samples



streamA :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data -> Signal dom Data
streamA enable x1 x2 = register 0 (mux enable (x1 + x2) oldVal)
    where oldVal = streamA enable x1 x2

streamB :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data -> Signal dom Data
streamB enable x1 x2 = register 0 (mux enable (x1 + x2) oldVal)
    where oldVal = streamB enable x1 x2

streamC :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data -> Signal dom Data
streamC enable x1 x2 = register 0 (mux enable (x1 + x2) oldVal)
    where oldVal = streamC enable x1 x2

streamD :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data -> Signal dom Data
streamD enable x1 x2 = register 0 (mux enable (x1 + x2) oldVal)
    where oldVal = streamD enable x1 x2

streamE :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data -> Signal dom Data
streamE enable x1 x2 = register 0 (mux enable (x1 + x2) oldVal)
    where oldVal = streamE enable x1 x2

streamF :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data -> Signal dom Data
streamF enable d e = register 0 (mux enable (d + e) oldVal)
    where oldVal = streamF enable d e

streamG :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data -> Signal dom Data
streamG enable b e = register 0 (mux enable (b + e) oldVal)
    where oldVal = streamG enable b e

streamH :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data -> Signal dom Data
streamH enable a c = register 0 (mux enable (a + c) oldVal)
    where oldVal = streamH enable a c

streamI :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data -> Signal dom Data
streamI enable a b = register 0 (mux enable (a + b) oldVal)
    where oldVal = streamI enable a b

streamJ :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data
streamJ enable a = register 0 (mux enable a oldVal)
    where oldVal = streamJ enable a

streamK :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data
streamK enable e = offsetVal
  where (_, _, offsetVal) = unbundle $ window3 enable e (-1)

streamL :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data
streamL enable k = register 0 (mux enable k oldVal)
    where oldVal = streamL enable k

streamM :: HiddenClockResetEnable dom => Signal dom Bool -> Signal dom Data -> Signal dom Data -> Signal dom Data
streamM enable j l = register 0 (mux enable (j + l) oldVal)
    where oldVal = streamM enable j l


----------------------------------------


topEntity :: Clock System -> Reset System -> Enable System ->
    Signal System Data -> Signal System Data -> Signal System Data ->
    Signal System Bool -> Signal System Bool -> Signal System Bool ->
  Signal System (Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data, Data)
topEntity clk rst en x1 x2 x3 hasX1 hasX2 hasX3 = bundle (a, b, c, d, e, f, g, h, i, j, k, l)
  where
    a = exposeClockResetEnable (streamA enableA holdAx1 holdAx2) clk rst en
    b = exposeClockResetEnable (streamB enableB holdBx1 holdBx2) clk rst en
    c = exposeClockResetEnable (streamC enableC holdCx1 holdCx2) clk rst en
    d = exposeClockResetEnable (streamD enableD holdDx1 holdDx2) clk rst en
    e = exposeClockResetEnable (streamE enableE holdEx1 holdEx2) clk rst en
    f = exposeClockResetEnable (streamF enableF d e) clk rst en
    g = exposeClockResetEnable (streamG enableG b e) clk rst en
    h = exposeClockResetEnable (streamH enableH a c) clk rst en
    i = exposeClockResetEnable (streamI enableI holdA holdB) clk rst en
    j = exposeClockResetEnable (streamJ enableJ holdA) clk rst en
    k = exposeClockResetEnable (streamK enableK e) clk rst en
    l = exposeClockResetEnable (streamL enableL k) clk rst en
    m = exposeClockResetEnable (streamM enableM j l) clk rst en

    enableA = exposeClockResetEnable (pacingX1andX2 hasX1 hasX2 hasX3) clk rst en
    enableB = exposeClockResetEnable (pacingX1orX2 hasX1 hasX2 hasX3) clk rst en
    enableC = exposeClockResetEnable (pacingX3 hasX1 hasX2 hasX3) clk rst en
    enableD = exposeClockResetEnable (pacingAny hasX1 hasX2 hasX3) clk rst en
    enableE = exposeClockResetEnable (pacingX1 hasX1 hasX2 hasX3) clk rst en
    enableF = exposeClockResetEnable (pacingX1 hasX1 hasX2 hasX3) clk rst en
    enableG = exposeClockResetEnable (pacingX1orX1andX2 hasX1 hasX2 hasX3) clk rst en
    enableH = exposeClockResetEnable (pacingAll hasX1 hasX2 hasX3) clk rst en
    enableI = exposeClockResetEnable pacing10kHz clk rst en
    enableJ = exposeClockResetEnable pacing10kHz clk rst en
    enableK = exposeClockResetEnable (pacingX1 hasX1 hasX2 hasX3) clk rst en
    enableL = exposeClockResetEnable pacing5kHz clk rst en
    enableM = exposeClockResetEnable pacing5kHz clk rst en

    holdAx1 = exposeClockResetEnable (register 0 x1) clk rst en
    holdAx2 = exposeClockResetEnable (register 0 x2) clk rst en
    holdBx1 = exposeClockResetEnable (register (-1) x1) clk rst en
    holdBx2 = exposeClockResetEnable (register (-1) x2) clk rst en
    holdCx1 = holdBx1
    holdCx2 = holdBx2
    holdDx1 = holdBx1
    holdDx2 = holdBx2
    holdEx1 = holdBx1
    holdEx2 = holdBx2
    holdA = exposeClockResetEnable (register (-1) a) clk rst en
    holdB = exposeClockResetEnable (register (-1) b) clk rst en




