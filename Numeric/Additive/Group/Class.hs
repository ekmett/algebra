module Numeric.Additive.Group.Class
  ( 
  -- * Additive Groups
    AdditiveGroup(..)
  ) where

import Data.Int
import Data.Word
import Prelude hiding ((+), (-), negate, subtract)
import qualified Prelude
import Numeric.Additive.Class
import Numeric.Additive.Monoid.Class

infixl 6 - 
infixl 7 `times`

class AdditiveMonoid r => AdditiveGroup r where
  (-)      :: r -> r -> r
  negate   :: r -> r
  subtract :: r -> r -> r
  times    :: Integral n => n -> r -> r
  times y0 x0 = case compare y0 0 of
    LT -> f (negate x0) (Prelude.negate y0)
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

  negate a = zero - a
  a - b  = a + negate b 
  subtract a b = negate a + b

instance AdditiveGroup r => AdditiveGroup (e -> r) where
  f - g = \x -> f x - g x
  negate f x = negate (f x)
  subtract f g x = subtract (f x) (g x)
  times n f e = times n (f e)

instance AdditiveGroup Integer where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance AdditiveGroup Int where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance AdditiveGroup Int8 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance AdditiveGroup Int16 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance AdditiveGroup Int32 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance AdditiveGroup Int64 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance AdditiveGroup Word where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance AdditiveGroup Word8 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance AdditiveGroup Word16 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance AdditiveGroup Word32 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance AdditiveGroup Word64 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r
