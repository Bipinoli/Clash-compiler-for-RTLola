-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Simple where

import Clash.Prelude


-- input x : Int
-- input y : Int
-- input z : Int

-- output a := x -> pacing @x
-- output b := y -> pacing @y
-- output c := a + b -> pacing @(x & y)
-- output d := a + b + z -> pacing @ (x & y & z)


------------ USING GADT --------------

-- -- GADT let's us store a value in a constructor and label that with a tag/type
-- -- 

-- -- input x : Int
-- -- input y : Int
-- -- output a := x + y

-- -- tags for the box
-- data PX deriving (Generic, NFDataX)
-- data PY deriving (Generic, NFDataX)
-- data PA deriving (Generic, NFDataX)

-- -- here 'a' is the Phantom type that is used to mark type eg. with PX, PY, PA
-- data Pacing a where
--     -- PacingX is a constructor that takes Bool param 
--     -- It produces a value of Pacing PX
--     -- And within the constructor it stores a value of Bool
--     -- so conceptually "Pacing PX" is a box labeled with PX that stores Bool inside it
--     PacingX :: Bool -> Pacing PX 
--     PacingY :: Bool -> Pacing PY
--     PacingA :: Pacing PX -> Pacing PY -> Pacing PA

-- getPacing :: Pacing a -> Bool
-- -- When we pattern match against "PacingX x" we are opening the box labeledd PX 
-- -- and extracting the boolean value `x` inside
-- getPacing (PacingX x) = x
-- getPacing (PacingY y) = y
-- getPacing (PacingA a b) = getPacing a && getPacing b


-- mkPacingX :: Bool -> Pacing PX
-- mkPacingX = PacingX

-- mkPacingY :: Bool -> Pacing PY
-- mkPacingY = PacingY

-- mkPacingA :: Pacing PX -> Pacing PY -> Pacing PA
-- mkPacingA = PacingA


-- func :: Pacing PA -> Bool
-- func pa = getPacing pa

-- px = mkPacingX True
-- py = mkPacingY False
-- pc = mkPacingA px py

-- zz = func pc


-- data Pacing a where
--     DirectPacing :: Bool -> Pacing Bool
--     DerivedPacing :: Pacing b -> Pacing Bool
--     DerivedPacingAnd :: KnownNat n => Vec n (Pacing b) -> Pacing Bool

-- getPacing :: Pacing a -> Bool
-- getPacing (DirectPacing b) = b
-- getPacing (DerivedPacing b) = getPacing b
-- getPacing (DerivedPacingAnd v) = and . map getPacing $ v

-- pacingX = DirectPacing True
-- pacingY = DirectPacing False
-- pacingZ = DirectPacing True

-- pacingA = DerivedPacing pacingX
-- pacingB = DerivedPacing pacingY

-- pacingC = DerivedPacingAnd (pacingA :> pacingB :> Nil)
-- pacingD = DerivedPacingAnd (pacingA :> pacingB :> pacingZ :> Nil)

-- px = getPacing pacingX
-- py = getPacing pacingY
-- pz = getPacing pacingZ
-- pa = getPacing pacingA
-- pb = getPacing pacingB
-- pc = getPacing pacingC
-- pd = getPacing pacingD

-- pacings = (px, py, pz, pa, pb, pc, pd)

-- type PacingD = DerivedPacing ( :> pacingB :> pacingZ :> Nil)



------------- USING normaal ADT -----------

-- input x : Int
-- input y : Int
-- output a := x + y

class Pacing a where 
    value :: a -> Bool

data PacingX = PacingX Bool 
    deriving (Generic, NFDataX)
data PacingY = PacingY Bool
    deriving (Generic, NFDataX)
data PacingA = PacingA PacingX PacingY
    deriving (Generic, NFDataX)

instance Pacing PacingX where
    value (PacingX x) = x
instance Pacing PacingY where
    value (PacingY y) = y
instance Pacing PacingA where
    value (PacingA x y) = value x && value y

func :: PacingA -> Bool
func = value 

px = PacingX True
py = PacingY False
pa = PacingA px py


-- class Pacing a where
--     value :: a -> Bool 

-- data PacingIn0 = PacingIn0 {
--     value :: Bool
-- } deriving (Generic, NFDataX)

-- data PacingIn1 = PacingIn1 {
--     value :: Bool
-- } deriving (Generic, NFDataX)

-- data PacingOut0 = PacingOut0 {
--     d :: PacingIn0
-- } deriving (Generic, NFDataX)

-- instance Pacing PacingOut0 where
--     value p = p.d.value

-- data PacingOut1 = PacingOut1 {
--     d :: PacingIn1
-- } deriving (Generic, NFDataX)

-- instance Pacing PacingOut1 where
    -- value p = p.d.value

-- data PacingOut2 = PacingOut2 {
--     value1 :: PacingOut0,
--     value2 :: PacingOut1
-- } deriving (Generic, NFDataX)

-- instance Pacing PacingOut2 where
--     pacing p = p.value1.pacing && p.value2.pacing