{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Ring.Rng
  ( RngRing(..)
  , rngRingHom
  , liftRngHom
  ) where

import Numeric.Addition
import Numeric.Module
import Numeric.Natural.Internal
import Numeric.Multiplication
import Numeric.Rig.Class
import Numeric.Rng.Class
import Numeric.Ring.Class
import Numeric.Semiring.Class
import Prelude hiding ((+),(-),(*),(/),replicate,negate,subtract,fromIntegral)

-- | The free Ring given a Rng obtained by adjoining Z, such that
-- 
-- > RngRing r = n*1 + r
--
-- This ring is commonly denoted r^.
data RngRing r = RngRing !Integer r deriving (Show,Read)

instance Abelian r => Additive (RngRing r) where
  RngRing n a + RngRing m b = RngRing (n + m) (a + b)
  replicate1p n (RngRing m a) = RngRing ((1 + toInteger n) * m) (replicate1p n a)

instance Abelian r => Abelian (RngRing r)

instance (Abelian r, AdditiveMonoid r) => LeftModule Natural (RngRing r) where
  n .* RngRing m a = RngRing (toInteger n * m) (replicate n a)

instance (Abelian r, AdditiveMonoid r) => RightModule Natural (RngRing r) where
  RngRing m a *. n = RngRing (toInteger n * m) (replicate n a)

instance (Abelian r, AdditiveMonoid r) => AdditiveMonoid (RngRing r) where
  zero = RngRing 0 zero
  replicate n (RngRing m a) = RngRing (toInteger n * m) (replicate n a)

instance (Abelian r, AdditiveGroup r) => LeftModule Integer (RngRing r) where
  n .* RngRing m a = RngRing (toInteger n * m) (times n a)

instance (Abelian r, AdditiveGroup r) => RightModule Integer (RngRing r) where
  RngRing m a *. n = RngRing (toInteger n * m) (times n a)

instance (Abelian r, AdditiveGroup r) => AdditiveGroup (RngRing r) where
  RngRing n a - RngRing m b = RngRing (n - m) (a - b)
  negate (RngRing n a) = RngRing (negate n) (negate a)
  subtract (RngRing n a) (RngRing m b) = RngRing (subtract n m) (subtract a b)
  times n (RngRing m a) = RngRing (toInteger n * m) (times n a)

instance Rng r => Multiplicative (RngRing r) where
  RngRing n a * RngRing m b = RngRing (n*m) (times n b + times m a + a * b)

instance (Commutative r, Rng r) => Commutative (RngRing r)

instance Rng s => LeftModule (RngRing s) (RngRing s) where
  (.*) = (*) 

instance Rng s => RightModule (RngRing s) (RngRing s) where
  (*.) = (*) 

instance Rng r => MultiplicativeMonoid (RngRing r) where
  one = RngRing 1 zero

instance (Rng r, MultiplicativeGroup r) => MultiplicativeGroup (RngRing r) where
  RngRing n a / RngRing m b = RngRing 0 $ (times n one + a) / (times m one + b)

instance Rng r => Semiring (RngRing r) 

instance Rng r => Rng (RngRing r)

instance Rng r => Rig (RngRing r)

instance Rng r => Ring (RngRing r)

-- | The rng homomorphism from r to RngRing r
rngRingHom :: r -> RngRing r
rngRingHom = RngRing 0

-- | given a rng homomorphism from a rng r into a ring s, liftRngHom yields a ring homomorphism from the ring `r^` into `s`.
liftRngHom :: Ring s => (r -> s) -> RngRing r -> s
liftRngHom g (RngRing n a) = times n one + g a
