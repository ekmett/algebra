module Numeric.Additive.Monoid.Class
  ( 
  -- * Additive Monoids
    AdditiveMonoid(..)
  , replicateMonoid
  , sum
  ) where

import Data.Foldable hiding (sum)
import Numeric.Additive.Class
import Data.Int
import Data.Word

import Prelude hiding ((+), sum)

-- | An additive monoid
--
-- > zero + a = a = a + zero
class Additive r => AdditiveMonoid r where
  zero :: r

  sumWith :: Foldable f => (a -> r) -> f a -> r
  sumWith f = foldl' (\b a -> b + f a) zero

sum :: (Foldable f, AdditiveMonoid r) => f r -> r
sum = sumWith id

replicateMonoid :: (Integral n, AdditiveMonoid r) => n -> r -> r
replicateMonoid y0 x0 = case compare y0 0 of
  LT -> error "replicateSemigroup: negative multiplier"
  EQ -> zero
  GT -> f x0 y0
  where
    f x y
      | even y = f (x + x) (y `quot` 2)
      | y == 1 = x
      | otherwise = g (x + x) ((y Prelude.- 1) `quot` 2) x
    g x y z
      | even y = g (x + x) (y `quot` 2) z
      | y == 1 = x + z
      | otherwise = g (x + x) ((y Prelude.- 1) `quot` 2) (x + z)

instance AdditiveMonoid r => AdditiveMonoid (e -> r) where
  zero = const zero
  sumWith f xs e = sumWith (`f` e) xs

instance AdditiveMonoid Integer where zero = 0
instance AdditiveMonoid Int where zero = 0
instance AdditiveMonoid Int8 where zero = 0
instance AdditiveMonoid Int16 where zero = 0
instance AdditiveMonoid Int32 where zero = 0
instance AdditiveMonoid Int64 where zero = 0
instance AdditiveMonoid Word where zero = 0
instance AdditiveMonoid Word8 where zero = 0
instance AdditiveMonoid Word16 where zero = 0
instance AdditiveMonoid Word32 where zero = 0
instance AdditiveMonoid Word64 where zero = 0

