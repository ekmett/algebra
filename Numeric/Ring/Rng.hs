module Numeric.Ring.Rng
  ( RngRing(..)
  , rngRingHom
  , liftRngHom
  ) where

import Numeric.Additive
import Numeric.Multiplicative
import Numeric.Rng.Class
import Numeric.Ring.Class
import Prelude hiding ((+),(-),(*),(/),replicate,negate,subtract)

-- | The free Ring given a Rng obtained by adjoining Z, such that
-- 
-- > RngRing r = n*1 + r
--
-- This ring is commonly denoted r^.
data RngRing r = RngRing !Integer r deriving (Show,Read)

instance Abelian r => Additive (RngRing r) where
  RngRing n a + RngRing m b = RngRing (n + m) (a + b)
  replicate n (RngRing m a) = RngRing (fromIntegral n * m) (replicate n a)

instance Abelian r => Abelian (RngRing r)

instance (Abelian r, AdditiveMonoid r) => AdditiveMonoid (RngRing r) where
  zero = RngRing 0 zero

instance (Abelian r, AdditiveGroup r) => AdditiveGroup (RngRing r) where
  RngRing n a - RngRing m b = RngRing (n - m) (a - b)
  negate (RngRing n a) = RngRing (negate n) (negate a)
  subtract (RngRing n a) (RngRing m b) = RngRing (subtract n m) (subtract a b)

instance Rng r => Multiplicative (RngRing r) where
  RngRing n a * RngRing m b = RngRing (n*m) (replicate n b + replicate m a + a * b)
  (^) = powMonoid -- if we know we have a group we can extend this to negative powers

instance (Commutative r, Rng r) => Commutative (RngRing r)

instance Rng r => MultiplicativeMonoid (RngRing r) where
  one = RngRing 1 zero

instance (Rng r, MultiplicativeGroup r) => MultiplicativeGroup (RngRing r) where
  RngRing n a / RngRing m b = RngRing 0 $ (replicate n one + a) / (replicate m one + b)

instance Rng r => Rng (RngRing r)

instance Rng r => Ring (RngRing r)

-- | The rng homomorphism from r to RngRing r
rngRingHom :: r -> RngRing r
rngRingHom = RngRing 0

-- | given a rng homomorphism from a rng r into a ring s, liftRngHom yields a ring homomorphism from the ring `r^` into `s`.
liftRngHom :: Ring s => (r -> s) -> RngRing r -> s
liftRngHom g (RngRing n a) = replicate n one + g a
