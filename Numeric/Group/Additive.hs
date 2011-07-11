{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Numeric.Group.Additive
  ( 
  -- * Additive Groups
    AdditiveGroup(..)
  ) where

import Data.Int
import Data.Word
import Prelude hiding ((+), (-), negate, subtract)
import qualified Prelude
import Numeric.Semigroup.Additive
import Numeric.Monoid.Additive
import Numeric.Module.Class

infixl 6 - 
infixl 7 `times`

class (LeftModule Integer r, RightModule Integer r, AdditiveMonoid r) => AdditiveGroup r where
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

instance AdditiveGroup () where 
  _ - _   = ()
  negate _ = ()
  subtract _ _  = ()
  times _ _   = ()

instance (AdditiveGroup a, AdditiveGroup b) => AdditiveGroup (a,b) where
  negate (a,b) = (negate a, negate b)
  (a,b) - (i,j) = (a-i, b-j)
  subtract (a,b) (i,j) = (subtract a i, subtract b j)
  times n (a,b) = (times n a,times n b)

instance (AdditiveGroup a, AdditiveGroup b, AdditiveGroup c) => AdditiveGroup (a,b,c) where
  negate (a,b,c) = (negate a, negate b, negate c)
  (a,b,c) - (i,j,k) = (a-i, b-j, c-k)
  subtract (a,b,c) (i,j,k) = (subtract a i, subtract b j, subtract c k)
  times n (a,b,c) = (times n a,times n b, times n c)

instance (AdditiveGroup a, AdditiveGroup b, AdditiveGroup c, AdditiveGroup d) => AdditiveGroup (a,b,c,d) where
  negate (a,b,c,d) = (negate a, negate b, negate c, negate d)
  (a,b,c,d) - (i,j,k,l) = (a-i, b-j, c-k, d-l)
  subtract (a,b,c,d) (i,j,k,l) = (subtract a i, subtract b j, subtract c k, subtract d l)
  times n (a,b,c,d) = (times n a,times n b, times n c, times n d)

instance (AdditiveGroup a, AdditiveGroup b, AdditiveGroup c, AdditiveGroup d, AdditiveGroup e) => AdditiveGroup (a,b,c,d,e) where
  negate (a,b,c,d,e) = (negate a, negate b, negate c, negate d, negate e)
  (a,b,c,d,e) - (i,j,k,l,m) = (a-i, b-j, c-k, d-l, e-m)
  subtract (a,b,c,d,e) (i,j,k,l,m) = (subtract a i, subtract b j, subtract c k, subtract d l, subtract e m)
  times n (a,b,c,d,e) = (times n a,times n b, times n c, times n d, times n e)
