module Numeric.Additive.Idempotent.Class
  ( 
  -- * Additive Monoids
    Idempotent
  , replicateIdempotent
  , replicateIdempotentMonoid
  ) where

import Numeric.Additive.Class
import Numeric.Additive.Monoid.Class

-- | An additive semigroup with idempotent addition.
--
-- > a + a = a
class Additive r => Idempotent r

replicateIdempotent :: (Integral n, Idempotent r) => n -> r -> r
replicateIdempotent n x 
  | n < 1 = error "replicateIdempotentSemigroup: positive multiplier required"
  | otherwise = x

replicateIdempotentMonoid :: (Integral n, Idempotent r, AdditiveMonoid r) => n -> r -> r
replicateIdempotentMonoid n x = case compare n 0 of
  LT -> error "replicateIdempotent: non-negative multiplier expected"
  EQ -> zero
  GT -> x

instance Idempotent Bool
instance Idempotent r => Idempotent (e -> r)
