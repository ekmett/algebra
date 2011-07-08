module Numeric.Rng.Zero
  ( ZeroRng(..)
  ) where

import Numeric.Rng.Class
import Numeric.Additive.Class
import Numeric.Additive.Idempotent.Class
import Numeric.Additive.Monoid.Class
import Numeric.Additive.Group.Class
import Numeric.Additive.Abelian.Class
import Numeric.Multiplicative.Class
import Numeric.Multiplicative.Band.Class
import Numeric.Semiring.Class
import Numeric.Multiplicative.Commutative.Class
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
  replicate n (ZeroRng a) = ZeroRng (replicate n a)

instance Idempotent r => Idempotent (ZeroRng r)

instance Abelian r => Abelian (ZeroRng r)

instance AdditiveMonoid r => AdditiveMonoid (ZeroRng r) where
  zero = ZeroRng zero
  sumWith f = ZeroRng . sumWith (runZeroRng . f)
  
instance AdditiveGroup r => AdditiveGroup (ZeroRng r) where
  ZeroRng a - ZeroRng b = ZeroRng (a - b)
  negate (ZeroRng a) = ZeroRng (negate a)
  subtract (ZeroRng a) (ZeroRng b) = ZeroRng (subtract a b)

instance AdditiveMonoid r => Multiplicative (ZeroRng r) where
  _ * _ = zero
  (^) = powSemigroup
  productWith1 f as = case toList as of
    [] -> error "productWith1: empty Foldable1"
    [a] -> f a
    _   -> zero

instance (Abelian r, AdditiveMonoid r) => Semiring (ZeroRng r)
instance AdditiveMonoid r => Commutative (ZeroRng r)
instance AdditiveMonoid r => Band (ZeroRng r) 
instance (AdditiveGroup r, Abelian r) => Rng (ZeroRng r)
