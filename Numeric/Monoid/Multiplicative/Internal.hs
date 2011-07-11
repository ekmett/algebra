{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Monoid.Multiplicative.Internal
  ( Unital(..)
  , product
  , FreeUnitalAlgebra(..)
  ) where

import Data.Foldable hiding (product)
import Data.Int
import Data.Word
import Prelude hiding ((*), foldr, product)
import Numeric.Semiring.Internal
import Numeric.Natural.Internal

infixr 8 `pow`

class Multiplicative r => Unital r where
  one :: r
  pow :: Whole n => r -> n -> r
  pow _ 0 = one
  pow x0 y0 = f x0 y0 where
    f x y 
      | even y = f (x * x) (y `quot` 2)
      | y == 1 = x
      | otherwise = g (x * x) ((y - 1) `quot` 2) x
    g x y z 
      | even y = g (x * x) (y `quot` 2) z
      | y == 1 = x * z
      | otherwise = g (x * x) ((y - 1) `quot` 2) (x * z)
  productWith :: Foldable f => (a -> r) -> f a -> r
  productWith f = foldl' (\b a -> b * f a) one

product :: (Foldable f, Unital r) => f r -> r
product = productWith id

instance Unital Bool where one = True
instance Unital Integer where one = 1
instance Unital Int where one = 1
instance Unital Int8 where one = 1
instance Unital Int16 where one = 1
instance Unital Int32 where one = 1
instance Unital Int64 where one = 1
instance Unital Natural where one = 1
instance Unital Word where one = 1
instance Unital Word8 where one = 1
instance Unital Word16 where one = 1
instance Unital Word32 where one = 1
instance Unital Word64 where one = 1
instance Unital () where one = ()
instance (Unital a, Unital b) => Unital (a,b) where
  one = (one,one)

instance (Unital a, Unital b, Unital c) => Unital (a,b,c) where
  one = (one,one,one)

instance (Unital a, Unital b, Unital c, Unital d) => Unital (a,b,c,d) where
  one = (one,one,one,one)

instance (Unital a, Unital b, Unital c, Unital d, Unital e) => Unital (a,b,c,d,e) where
  one = (one,one,one,one,one)

-- | An associative unital algebra over a semiring, built using a free module
class (Unital r, FreeAlgebra r a) => FreeUnitalAlgebra r a where
  unit :: r -> a -> r

instance (FreeUnitalAlgebra r a) => Unital (a -> r) where
  one = unit one

instance FreeUnitalAlgebra () a where
  unit _ _ = ()

instance (FreeUnitalAlgebra r a, FreeUnitalAlgebra r b) => FreeUnitalAlgebra (a -> r) b where
  unit f b a = unit (f a) b

instance (FreeUnitalAlgebra r a, FreeUnitalAlgebra r b) => FreeUnitalAlgebra r (a,b) where
  unit r (a,b) = unit r a * unit r b

instance (FreeUnitalAlgebra r a, FreeUnitalAlgebra r b, FreeUnitalAlgebra r c) => FreeUnitalAlgebra r (a,b,c) where
  unit r (a,b,c) = unit r a * unit r b * unit r c

instance (FreeUnitalAlgebra r a, FreeUnitalAlgebra r b, FreeUnitalAlgebra r c, FreeUnitalAlgebra r d) => FreeUnitalAlgebra r (a,b,c,d) where
  unit r (a,b,c,d) = unit r a * unit r b * unit r c * unit r d

instance (FreeUnitalAlgebra r a, FreeUnitalAlgebra r b, FreeUnitalAlgebra r c, FreeUnitalAlgebra r d, FreeUnitalAlgebra r e) => FreeUnitalAlgebra r (a,b,c,d,e) where
  unit r (a,b,c,d,e) = unit r a * unit r b * unit r c * unit r d * unit r e
