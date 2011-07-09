module Numeric.Addition.Idempotent
  ( 
  -- * Additive Monoids
    Idempotent
  , replicate1pIdempotent
  , replicateIdempotent
  ) where

import Numeric.Semigroup.Additive
import Numeric.Monoid.Additive
import Numeric.Natural.Internal

-- | An additive semigroup with idempotent addition.
--
-- > a + a = a
--
-- An (Idempotent r, Rig r) => r is also known as a dioid
class Additive r => Idempotent r

replicate1pIdempotent :: Natural -> r -> r
replicate1pIdempotent _ r = r

replicateIdempotent :: (Integral n, Idempotent r, AdditiveMonoid r) => n -> r -> r
replicateIdempotent 0 _ = zero
replicateIdempotent _ x = x

instance Idempotent ()
instance Idempotent Bool
instance Idempotent r => Idempotent (e -> r)
instance (Idempotent a, Idempotent b) => Idempotent (a,b)
instance (Idempotent a, Idempotent b, Idempotent c) => Idempotent (a,b,c)
instance (Idempotent a, Idempotent b, Idempotent c, Idempotent d) => Idempotent (a,b,c,d)
instance (Idempotent a, Idempotent b, Idempotent c, Idempotent d, Idempotent e) => Idempotent (a,b,c,d,e)
