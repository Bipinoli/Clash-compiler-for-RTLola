-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GADTs #-}

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

data PacingX = PacingX Bool
data PacingY = PacingY Bool


data Pacing a where
    DirectPacing :: Bool -> Pacing Bool
    DerivedPacing :: Pacing b -> Pacing Bool
    DerivedPacingAnd :: KnownNat n => Vec n (Pacing b) -> Pacing Bool

getPacing :: Pacing a -> Bool
getPacing (DirectPacing b) = b
getPacing (DerivedPacing b) = getPacing b
getPacing (DerivedPacingAnd v) = and . map getPacing $ v

pacingX = DirectPacing True
pacingY = DirectPacing False
pacingZ = DirectPacing True

pacingA = DerivedPacing pacingX
pacingB = DerivedPacing pacingY

pacingC = DerivedPacingAnd (pacingA :> pacingB :> Nil)
pacingD = DerivedPacingAnd (pacingA :> pacingB :> pacingZ :> Nil)

px = getPacing pacingX
py = getPacing pacingY
pz = getPacing pacingZ
pa = getPacing pacingA
pb = getPacing pacingB
pc = getPacing pacingC
pd = getPacing pacingD

pacings = (px, py, pz, pa, pb, pc, pd)

-- type PacingD = DerivedPacing ( :> pacingB :> pacingZ :> Nil)

-- func :: PacingD -> Bool
-- func pacingD = getPacing pacingD


------------- USING RECORD -----------


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