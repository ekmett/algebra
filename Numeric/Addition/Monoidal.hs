{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Numeric.Addition.Monoidal
  ( 
  -- * Additive Monoids
    Monoidal(..)
  , sum
  ) where

import Data.Foldable hiding (sum)
import Data.Int
import Data.Word
import Numeric.Module.Class
import Numeric.Natural.Internal
import Numeric.Additive
import Prelude hiding ((+), sum, replicate)

-- | An additive monoid
--
-- > zero + a = a = a + zero
class (LeftModule Natural m, RightModule Natural m) => Monoidal m where
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

sum :: (Foldable f, Monoidal m) => f m -> m
sum = sumWith id

instance Monoidal Bool where 
  zero = False
  replicate 0 _ = False
  replicate _ r = r

instance Monoidal Natural where
  zero = 0
  replicate n r = toNatural n * r

instance Monoidal Integer where 
  zero = 0
  replicate n r = toInteger n * r

instance Monoidal Int where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Int8 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Int16 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Int32 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Int64 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Word where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Word8 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Word16 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Word32 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Word64 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal r => Monoidal (e -> r) where
  zero = const zero
  sumWith f xs e = sumWith (`f` e) xs
  replicate n r e = replicate n (r e)

instance Monoidal () where 
  zero = ()
  replicate _ () = ()
  sumWith _ _ = ()

instance (Monoidal a, Monoidal b) => Monoidal (a,b) where
  zero = (zero,zero)
  replicate n (a,b) = (replicate n a, replicate n b)

instance (Monoidal a, Monoidal b, Monoidal c) => Monoidal (a,b,c) where
  zero = (zero,zero,zero)
  replicate n (a,b,c) = (replicate n a, replicate n b, replicate n c)

instance (Monoidal a, Monoidal b, Monoidal c, Monoidal d) => Monoidal (a,b,c,d) where
  zero = (zero,zero,zero,zero)
  replicate n (a,b,c,d) = (replicate n a, replicate n b, replicate n c, replicate n d)

instance (Monoidal a, Monoidal b, Monoidal c, Monoidal d, Monoidal e) => Monoidal (a,b,c,d,e) where
  zero = (zero,zero,zero,zero,zero)
  replicate n (a,b,c,d,e) = (replicate n a, replicate n b, replicate n c, replicate n d, replicate n e)
