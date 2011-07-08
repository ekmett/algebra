module Numeric.Additive.Class
  ( 
  -- * Additive Semigroups
    Additive(..)
  , replicateAdditive 
  , sum1
  ) where

import qualified Prelude
import Prelude hiding ((+), replicate)
import Data.Int
import Data.Word
import Data.Semigroup.Foldable
import Data.Foldable

infixl 6 +

-- | 
-- > (a + b) + c = a + (b + c)
-- > replicate 1 a = a
-- > replicate (2 * n) a = replicate n a + replicate n a
-- > replicate (2 * n + 1) a = replicate n a + replicate n a + a
class Additive r where
  (+) :: r -> r -> r

  replicate :: Integral n => n -> r -> r

  sumWith1 :: Foldable1 f => (a -> r) -> f a -> r
  sumWith1 f = maybe (error "Numeric.Additive.Semigroup.sumWith1: empty structure") id . foldl' mf Nothing
     where mf Nothing y = Just $! f y 
           mf (Just x) y = Just $! x + f y

-- | A suitable default definition for replicate, given only a semigroup.
--   Not used as a default definition, because you can usually do better if you have more than a semigroup!
replicateAdditive :: (Integral n, Additive r) => n -> r -> r
replicateAdditive y0 x0 = case compare y0 0 of
  LT -> error "replicateAdditive: negative multiplier"
  EQ -> error "replicateAdditive: zero multiplier"
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

sum1 :: (Foldable1 f, Additive r) => f r -> r
sum1 = sumWith1 id

instance Additive r => Additive (b -> r) where
  f + g = \e -> f e + g e 
  replicate n f e = replicate n (f e)
  sumWith1 f xs e = sumWith1 (`f` e) xs

instance Additive Integer where 
  (+) = (Prelude.+)
  replicate n r = fromIntegral n * r

instance Additive Int where
  (+) = (Prelude.+)
  replicate n r = fromIntegral n * r

instance Additive Int8 where
  (+) = (Prelude.+)
  replicate n r = fromIntegral n * r

instance Additive Int16 where
  (+) = (Prelude.+)
  replicate n r = fromIntegral n * r

instance Additive Int32 where
  (+) = (Prelude.+)
  replicate n r = fromIntegral n * r

instance Additive Int64 where
  (+) = (Prelude.+)
  replicate n r = fromIntegral n * r

instance Additive Word where
  (+) = (Prelude.+)
  replicate n r = fromIntegral n * r

instance Additive Word8 where
  (+) = (Prelude.+)
  replicate n r = fromIntegral n * r

instance Additive Word16 where
  (+) = (Prelude.+)
  replicate n r = fromIntegral n * r

instance Additive Word32 where
  (+) = (Prelude.+)
  replicate n r = fromIntegral n * r

instance Additive Word64 where
  (+) = (Prelude.+)
  replicate n r = fromIntegral n * r

