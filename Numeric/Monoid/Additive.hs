module Numeric.Monoid.Additive
  ( 
  -- * Additive Monoids
    AdditiveMonoid(..)
  , sum
  ) where

import Data.Foldable hiding (sum)
import Numeric.Semigroup.Additive
import Data.Int
import Data.Word
import Numeric.Natural.Internal

import Prelude hiding ((+), sum, replicate)

-- | An additive monoid
--
-- > zero + a = a = a + zero
class Additive r => AdditiveMonoid r where
  zero :: r

  replicate :: (Whole n, AdditiveMonoid r) => n -> r -> r
  replicate 0 _  = zero
  replicate n x0 = f x0 n
    where
      f x y
        | even y = f (x + x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x + x) (unsafePred y `quot` 2) x
      g x y z
        | even y = g (x + x) (y `quot` 2) z
        | y == 1 = x + z
        | otherwise = g (x + x) (unsafePred y `quot` 2) (x + z)

  sumWith :: Foldable f => (a -> r) -> f a -> r
  sumWith f = foldl' (\b a -> b + f a) zero

sum :: (Foldable f, AdditiveMonoid r) => f r -> r
sum = sumWith id

instance AdditiveMonoid r => AdditiveMonoid (e -> r) where
  zero = const zero
  sumWith f xs e = sumWith (`f` e) xs
  replicate n r e = replicate n (r e)

instance AdditiveMonoid () where 
  zero = ()
  replicate _ () = ()
  sumWith f _ = ()

instance AdditiveMonoid Bool where 
  zero = False
  replicate 0 _ = False
  replicate _ r = r

instance AdditiveMonoid Natural where
  zero = 0
  replicate n r = toNatural n * r

instance AdditiveMonoid Integer where 
  zero = 0
  replicate n r = toInteger n * r

instance AdditiveMonoid Int where 
  zero = 0
  replicate n r = fromIntegral n * r

instance AdditiveMonoid Int8 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance AdditiveMonoid Int16 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance AdditiveMonoid Int32 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance AdditiveMonoid Int64 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance AdditiveMonoid Word where 
  zero = 0
  replicate n r = fromIntegral n * r

instance AdditiveMonoid Word8 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance AdditiveMonoid Word16 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance AdditiveMonoid Word32 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance AdditiveMonoid Word64 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance (AdditiveMonoid a, AdditiveMonoid b) => AdditiveMonoid (a,b) where
  zero = (zero,zero)
  replicate n (a,b) = (replicate n a, replicate n b)

instance (AdditiveMonoid a, AdditiveMonoid b, AdditiveMonoid c) => AdditiveMonoid (a,b,c) where
  zero = (zero,zero,zero)
  replicate n (a,b,c) = (replicate n a, replicate n b, replicate n c)

instance (AdditiveMonoid a, AdditiveMonoid b, AdditiveMonoid c, AdditiveMonoid d) => AdditiveMonoid (a,b,c,d) where
  zero = (zero,zero,zero,zero)
  replicate n (a,b,c,d) = (replicate n a, replicate n b, replicate n c, replicate n d)

instance (AdditiveMonoid a, AdditiveMonoid b, AdditiveMonoid c, AdditiveMonoid d, AdditiveMonoid e) => AdditiveMonoid (a,b,c,d,e) where
  zero = (zero,zero,zero,zero,zero)
  replicate n (a,b,c,d,e) = (replicate n a, replicate n b, replicate n c, replicate n d, replicate n e)


