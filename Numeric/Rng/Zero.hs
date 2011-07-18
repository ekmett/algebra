{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Numeric.Rng.Zero
  ( ZeroRng(..)
  ) where

import Numeric.Algebra
import Data.Foldable (toList)
import Prelude hiding ((+),(-),negate,subtract,replicate)

-- *** The Zero Rng for an Abelian Group, adding the trivial product
--
-- > _ * _ = zero 
--
-- which distributes over (+)

-- ZeroRng/runZeroRng witness an additive Abelian group isomorphism to the zero rng.
newtype ZeroRng r = ZeroRng { runZeroRng :: r } deriving (Eq,Ord,Show,Read)

instance Additive r => Additive (ZeroRng r) where
  ZeroRng a + ZeroRng b = ZeroRng (a + b)
  sumWith1 f = ZeroRng . sumWith1 (runZeroRng . f)

instance Idempotent r => Idempotent (ZeroRng r)

instance Abelian r => Abelian (ZeroRng r)

instance Monoidal r => Monoidal (ZeroRng r) where
  zero = ZeroRng zero
  sumWith f = ZeroRng . sumWith (runZeroRng . f)
  replicate n (ZeroRng a) = ZeroRng (replicate n a)
  
instance Group r => Group (ZeroRng r) where
  ZeroRng a - ZeroRng b = ZeroRng (a - b)
  negate (ZeroRng a) = ZeroRng (negate a)
  subtract (ZeroRng a) (ZeroRng b) = ZeroRng (subtract a b)
  times n (ZeroRng a) = ZeroRng (times n a)

instance Monoidal r => Multiplicative (ZeroRng r) where
  _ * _ = zero
  productWith1 f as = case toList as of
    [] -> error "productWith1: empty Foldable1"
    [a] -> f a
    _   -> zero

instance (Monoidal r, Abelian r) => Semiring (ZeroRng r)
instance Monoidal r => Commutative (ZeroRng r)
instance (Group r, Abelian r) => Rng (ZeroRng r)
instance Monoidal r => LeftModule Natural (ZeroRng r) where
  (.*) = replicate
instance Monoidal r => RightModule Natural (ZeroRng r) where
  m *. n = replicate n m
instance Group r => LeftModule Integer (ZeroRng r) where
  (.*) = times
instance Group r => RightModule Integer (ZeroRng r) where
  m *. n = times n m
