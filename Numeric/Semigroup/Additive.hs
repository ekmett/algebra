module Numeric.Semigroup.Additive
  ( 
  -- * Additive Semigroups
    Additive(..)
  , sum1
  ) where

import qualified Prelude
import Prelude hiding ((+), replicate)
import Data.Int
import Data.Word
import Data.Semigroup.Foldable
import Data.Foldable
import Numeric.Natural.Internal

infixl 6 +

-- | 
-- > (a + b) + c = a + (b + c)
-- > replicate 1 a = a
-- > replicate (2 * n) a = replicate n a + replicate n a
-- > replicate (2 * n + 1) a = replicate n a + replicate n a + a
class Additive r where
  (+) :: r -> r -> r

  -- | replicate1p n r = replicate (1 + n) r
  replicate1p :: Whole n => n -> r -> r
  replicate1p y0 x0 = f x0 (1 Prelude.+ y0)
    where
      f x y
        | even y = f (x + x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x + x) (unsafePred y  `quot` 2) x
      g x y z
        | even y = g (x + x) (y `quot` 2) z
        | y == 1 = x + z
        | otherwise = g (x + x) (unsafePred y `quot` 2) (x + z)

  sumWith1 :: Foldable1 f => (a -> r) -> f a -> r
  sumWith1 f = maybe (error "Numeric.Additive.Semigroup.sumWith1: empty structure") id . foldl' mf Nothing
     where mf Nothing y = Just $! f y 
           mf (Just x) y = Just $! x + f y

sum1 :: (Foldable1 f, Additive r) => f r -> r
sum1 = sumWith1 id

instance Additive r => Additive (b -> r) where
  f + g = \e -> f e + g e 
  replicate1p n f e = replicate1p n (f e)
  sumWith1 f xs e = sumWith1 (`f` e) xs

instance Additive Bool where
  (+) = (||)
  replicate1p _ a = a

instance Additive Natural where
  (+) = (Prelude.+)
  replicate1p n r = (1 Prelude.+ toNatural n) * r

instance Additive Integer where 
  (+) = (Prelude.+)
  replicate1p n r = (1 Prelude.+ toInteger n) * r

instance Additive Int where
  (+) = (Prelude.+)
  replicate1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Int8 where
  (+) = (Prelude.+)
  replicate1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Int16 where
  (+) = (Prelude.+)
  replicate1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Int32 where
  (+) = (Prelude.+)
  replicate1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Int64 where
  (+) = (Prelude.+)
  replicate1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Word where
  (+) = (Prelude.+)
  replicate1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Word8 where
  (+) = (Prelude.+)
  replicate1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Word16 where
  (+) = (Prelude.+)
  replicate1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Word32 where
  (+) = (Prelude.+)
  replicate1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Word64 where
  (+) = (Prelude.+)
  replicate1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive () where
  _ + _ = ()
  replicate1p _ _ = () 
  sumWith1 _ _ = ()

instance (Additive a, Additive b) => Additive (a,b) where
  (a,b) + (i,j) = (a + i, b + j)
  replicate1p n (a,b) = (replicate1p n a, replicate1p n b)

instance (Additive a, Additive b, Additive c) => Additive (a,b,c) where
  (a,b,c) + (i,j,k) = (a + i, b + j, c + k)
  replicate1p n (a,b,c) = (replicate1p n a, replicate1p n b, replicate1p n c)

instance (Additive a, Additive b, Additive c, Additive d) => Additive (a,b,c,d) where
  (a,b,c,d) + (i,j,k,l) = (a + i, b + j, c + k, d + l)
  replicate1p n (a,b,c,d) = (replicate1p n a, replicate1p n b, replicate1p n c, replicate1p n d)

instance (Additive a, Additive b, Additive c, Additive d, Additive e) => Additive (a,b,c,d,e) where
  (a,b,c,d,e) + (i,j,k,l,m) = (a + i, b + j, c + k, d + l, e + m)
  replicate1p n (a,b,c,d,e) = (replicate1p n a, replicate1p n b, replicate1p n c, replicate1p n d, replicate1p n e)
