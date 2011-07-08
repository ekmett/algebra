module Numeric.Additive.Idempotent.Class
  ( 
  -- * Additive Monoids
    Idempotent
  , replicate1pIdempotent
  , replicateIdempotent
  ) where

import Numeric.Additive.Class
import Numeric.Additive.Monoid.Class
import Numeric.Natural.Internal

-- | An additive semigroup with idempotent addition.
--
-- > a + a = a
class Additive r => Idempotent r

replicate1pIdempotent :: Natural -> r -> r
replicate1pIdempotent _ r = r

replicateIdempotent :: (Integral n, Idempotent r, AdditiveMonoid r) => n -> r -> r
replicateIdempotent 0 _ = zero
replicateIdempotent _ x = x

instance Idempotent Bool
instance Idempotent r => Idempotent (e -> r)
