module Numeric.Ring.Opposite 
  ( Opposite(..)
  ) where

import Data.Foldable
import Data.Function (on)
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Traversable
import Numeric.Additive
import Numeric.Multiplicative
import Numeric.Rng.Class
import Numeric.Ring.Class
import Numeric.Semiring.Class
import Numeric.Decidable.Associates
import Numeric.Decidable.Units
import Numeric.Decidable.Zero
import Prelude hiding ((-),(+),(*),(/),(^),recip,negate,subtract,replicate)

-- | http://en.wikipedia.org/wiki/Opposite_ring
newtype Opposite r = Opposite { runOpposite :: r } deriving (Show,Read)
instance Eq r => Eq (Opposite r) where
  (==) = (==) `on` runOpposite
instance Ord r => Ord (Opposite r) where
  compare = compare `on` runOpposite
instance Functor Opposite where
  fmap f (Opposite r) = Opposite (f r)
instance Foldable Opposite where
  foldMap f (Opposite r) = f r
instance Traversable Opposite where
  traverse f (Opposite r) = fmap Opposite (f r)
instance Foldable1 Opposite where
  foldMap1 f (Opposite r) = f r
instance Traversable1 Opposite where
  traverse1 f (Opposite r) = fmap Opposite (f r)
instance Additive r => Additive (Opposite r) where
  Opposite a + Opposite b = Opposite (a + b)
  replicate n (Opposite a) = Opposite (replicate n a)
  sumWith1 f = Opposite . sumWith1 (runOpposite . f)
instance AdditiveMonoid r => AdditiveMonoid (Opposite r) where
  zero = Opposite zero
  sumWith f = Opposite . sumWith (runOpposite . f)
instance AdditiveGroup r => AdditiveGroup (Opposite r) where
  negate = Opposite . negate . runOpposite
  Opposite a - Opposite b = Opposite (a - b)
  subtract (Opposite a) (Opposite b) = Opposite (subtract a b)
instance Abelian r => Abelian (Opposite r)
instance DecidableZero r => DecidableZero (Opposite r) where
  isZero = isZero . runOpposite
instance DecidableUnits r => DecidableUnits (Opposite r) where
  recipUnit = fmap Opposite . recipUnit . runOpposite
instance DecidableAssociates r => DecidableAssociates (Opposite r) where
  isAssociate (Opposite a) (Opposite b) = isAssociate a b
instance Multiplicative r => Multiplicative (Opposite r) where
  Opposite a * Opposite b = Opposite (b * a)
  Opposite a ^ n = Opposite (a ^ n)
instance Commutative r => Commutative (Opposite r)
instance Idempotent r => Idempotent (Opposite r)
instance Band r => Band (Opposite r)
instance MultiplicativeMonoid r => MultiplicativeMonoid (Opposite r) where
  one = Opposite one
instance MultiplicativeGroup r => MultiplicativeGroup (Opposite r) where
  recip = Opposite . recip . runOpposite
  Opposite a / Opposite b = Opposite (b \\ a)
  Opposite a \\ Opposite b = Opposite (b / a)
instance Semiring r => Semiring (Opposite r)
instance Rng r => Rng (Opposite r)
instance Ring r => Ring (Opposite r)
