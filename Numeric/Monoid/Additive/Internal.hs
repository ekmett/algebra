{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module Numeric.Monoid.Additive.Internal
  ( 
  -- * Additive Monoids
    AdditiveMonoid(..)
  , sum
  , LeftModule(..)
  , RightModule(..)
  ) where

import Data.Foldable hiding (sum)
import Numeric.Semigroup.Additive
import Numeric.Semigroup.Multiplicative.Internal
import Data.Int
import Data.Word
import Numeric.Natural.Internal

import Prelude hiding ((+), (*), sum, replicate)

class (Semiring r, Additive m) => LeftModule r m where
  (.*) :: r -> m -> m

instance LeftModule Natural Bool where (.*) = replicate
instance LeftModule Natural Natural where (.*) = (*)
instance LeftModule Natural Integer where Natural n .* m = n * m
instance LeftModule Integer Integer where (.*) = (*) 
instance LeftModule Natural Int where (.*) = (*) . fromIntegral
instance LeftModule Integer Int where (.*) = (*) . fromInteger
instance LeftModule Natural Int8 where (.*) = (*) . fromIntegral
instance LeftModule Integer Int8 where (.*) = (*) . fromInteger
instance LeftModule Natural Int16 where (.*) = (*) . fromIntegral
instance LeftModule Integer Int16 where (.*) = (*) . fromInteger
instance LeftModule Natural Int32 where (.*) = (*) . fromIntegral
instance LeftModule Integer Int32 where (.*) = (*) . fromInteger
instance LeftModule Natural Int64 where (.*) = (*) . fromIntegral
instance LeftModule Integer Int64 where (.*) = (*) . fromInteger
instance LeftModule Natural Word where (.*) = (*) . fromIntegral
instance LeftModule Integer Word where (.*) = (*) . fromInteger
instance LeftModule Natural Word8 where (.*) = (*) . fromIntegral
instance LeftModule Integer Word8 where (.*) = (*) . fromInteger
instance LeftModule Natural Word16 where (.*) = (*) . fromIntegral
instance LeftModule Integer Word16 where (.*) = (*) . fromInteger
instance LeftModule Natural Word32 where (.*) = (*) . fromIntegral
instance LeftModule Integer Word32 where (.*) = (*) . fromInteger
instance LeftModule Natural Word64 where (.*) = (*) . fromIntegral
instance LeftModule Integer Word64 where (.*) = (*) . fromInteger
instance Semiring r => LeftModule r () where _ .* _ = ()
instance LeftModule r m => LeftModule r (e -> m) where (.*) m f e = m .* f e
instance (LeftModule r a, LeftModule r b) => LeftModule r (a, b) where
  n .* (a, b) = (n .* a, n .* b)
instance (LeftModule r a, LeftModule r b, LeftModule r c) => LeftModule r (a, b, c) where
  n .* (a, b, c) = (n .* a, n .* b, n .* c)
instance (LeftModule r a, LeftModule r b, LeftModule r c, LeftModule r d) => LeftModule r (a, b, c, d) where
  n .* (a, b, c, d) = (n .* a, n .* b, n .* c, n .* d)
instance (LeftModule r a, LeftModule r b, LeftModule r c, LeftModule r d, LeftModule r e) => LeftModule r (a, b, c, d, e) where
  n .* (a, b, c, d, e) = (n .* a, n .* b, n .* c, n .* d, n .* e)

class (Semiring r, Additive m) => RightModule r m where
  (*.) :: m -> r -> m

instance RightModule Natural Bool where (*.) = flip replicate
instance RightModule Natural Natural where (*.) = (*)
instance RightModule Natural Integer where n *. Natural m = n * m
instance RightModule Integer Integer where (*.) = (*) 
instance RightModule Natural Int where m *. n = m * fromIntegral n
instance RightModule Integer Int where m *. n = m * fromInteger n
instance RightModule Natural Int8 where m *. n = m * fromIntegral n
instance RightModule Integer Int8 where m *. n = m * fromInteger n
instance RightModule Natural Int16 where m *. n = m * fromIntegral n
instance RightModule Integer Int16 where m *. n = m * fromInteger n
instance RightModule Natural Int32 where m *. n = m * fromIntegral n
instance RightModule Integer Int32 where m *. n = m * fromInteger n
instance RightModule Natural Int64 where m *. n = m * fromIntegral n
instance RightModule Integer Int64 where m *. n = m * fromInteger n
instance RightModule Natural Word where m *. n = m * fromIntegral n
instance RightModule Integer Word where m *. n = m * fromInteger n
instance RightModule Natural Word8 where m *. n = m * fromIntegral n
instance RightModule Integer Word8 where m *. n = m * fromInteger n
instance RightModule Natural Word16 where m *. n = m * fromIntegral n
instance RightModule Integer Word16 where m *. n = m * fromInteger n
instance RightModule Natural Word32 where m *. n = m * fromIntegral n
instance RightModule Integer Word32 where m *. n = m * fromInteger n
instance RightModule Natural Word64 where m *. n = m * fromIntegral n
instance RightModule Integer Word64 where m *. n = m * fromInteger n
instance Semiring r => RightModule r () where _ *. _ = ()
instance RightModule r m => RightModule r (e -> m) where (*.) f m e = f e *. m
instance (RightModule r a, RightModule r b) => RightModule r (a, b) where
  (a, b) *. n = (a *. n, b *. n)
instance (RightModule r a, RightModule r b, RightModule r c) => RightModule r (a, b, c) where
  (a, b, c) *. n = (a *. n, b *. n, c *. n)
instance (RightModule r a, RightModule r b, RightModule r c, RightModule r d) => RightModule r (a, b, c, d) where
  (a, b, c, d) *. n = (a *. n, b *. n, c *. n, d *. n)
instance (RightModule r a, RightModule r b, RightModule r c, RightModule r d, RightModule r e) => RightModule r (a, b, c, d, e) where
  (a, b, c, d, e) *. n = (a *. n, b *. n, c *. n, d *. n, e *. n)

-- | An additive monoid
--
-- > zero + a = a = a + zero
class (LeftModule Natural m, RightModule Natural m) => AdditiveMonoid m where
  zero :: m

  replicate :: Whole n => n -> m -> m
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

  sumWith :: Foldable f => (a -> m) -> f a -> m
  sumWith f = foldl' (\b a -> b + f a) zero

sum :: (Foldable f, AdditiveMonoid m) => f m -> m
sum = sumWith id

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

instance AdditiveMonoid () where 
  zero = ()
  replicate _ () = ()
  sumWith _ _ = ()

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

instance AdditiveMonoid r => AdditiveMonoid (e -> r) where
  zero = const zero
  sumWith f xs e = sumWith (`f` e) xs
  replicate n r e = replicate n (r e)
