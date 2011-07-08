module Numeric.Rng.Zero 
  ( ZeroRng(..)
  ) where

import Numeric.Rng.Class
import Numeric.Additive
import Numeric.Multiplicative.Semigroup
import Data.Foldable (toList)
import Prelude hiding ((+),(-),negate,subtract,replicate)

-- *** The Zero Rng for an Abelian Group, adding the trivial product
--
-- > _ * _ = zero 
--
-- which distributes over (+)

-- ZeroRng/runZeroRng witness an additive Abelian group isomorphism to the zero rng.
newtype ZeroRng r = ZeroRng { runZeroRng :: r } deriving (Eq,Ord,Show,Read)

instance AdditiveSemigroup r => AdditiveSemigroup (ZeroRng r) where
  ZeroRng a + ZeroRng b = ZeroRng (a + b)
  sumWith1 f = ZeroRng . sumWith1 (runZeroRng . f)
  replicate n (ZeroRng a) = ZeroRng (replicate n a)

instance AdditiveMonoid r => AdditiveMonoid (ZeroRng r) where
  zero = ZeroRng zero
  sumWith f = ZeroRng . sumWith (runZeroRng . f)
  
instance AdditiveGroup r => AdditiveGroup (ZeroRng r) where
  ZeroRng a - ZeroRng b = ZeroRng (a - b)
  negate (ZeroRng a) = ZeroRng (negate a)
  subtract (ZeroRng a) (ZeroRng b) = ZeroRng (subtract a b)

instance AdditiveAbelianGroup r => AdditiveAbelianGroup (ZeroRng r)

instance AdditiveMonoid r => MultiplicativeSemigroup (ZeroRng r) where
  _ * _ = zero
  (^) = powSemigroup
  productWith1 f as = case toList as of
    [] -> error "productWith1: empty Foldable1"
    [a] -> f a
    _   -> zero

instance AdditiveAbelianGroup r => Rng (ZeroRng r)
