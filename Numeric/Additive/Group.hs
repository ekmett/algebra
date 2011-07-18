{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Numeric.Additive.Group
  ( -- * Additive Groups
    Group(..)
  ) where

import Data.Int
import Data.Word
import Prelude hiding ((*), (+), (-), negate, subtract)
import qualified Prelude
import Numeric.Additive.Class
import Numeric.Algebra.Class

infixl 6 - 
infixl 7 `times`

class (LeftModule Integer r, RightModule Integer r, Monoidal r) => Group r where
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

instance Group r => Group (e -> r) where
  f - g = \x -> f x - g x
  negate f x = negate (f x)
  subtract f g x = subtract (f x) (g x)
  times n f e = times n (f e)

instance Group Integer where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance Group Int where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance Group Int8 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance Group Int16 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance Group Int32 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance Group Int64 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance Group Word where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance Group Word8 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance Group Word16 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance Group Word32 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance Group Word64 where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance Group () where 
  _ - _   = ()
  negate _ = ()
  subtract _ _  = ()
  times _ _   = ()

instance (Group a, Group b) => Group (a,b) where
  negate (a,b) = (negate a, negate b)
  (a,b) - (i,j) = (a-i, b-j)
  subtract (a,b) (i,j) = (subtract a i, subtract b j)
  times n (a,b) = (times n a,times n b)

instance (Group a, Group b, Group c) => Group (a,b,c) where
  negate (a,b,c) = (negate a, negate b, negate c)
  (a,b,c) - (i,j,k) = (a-i, b-j, c-k)
  subtract (a,b,c) (i,j,k) = (subtract a i, subtract b j, subtract c k)
  times n (a,b,c) = (times n a,times n b, times n c)

instance (Group a, Group b, Group c, Group d) => Group (a,b,c,d) where
  negate (a,b,c,d) = (negate a, negate b, negate c, negate d)
  (a,b,c,d) - (i,j,k,l) = (a-i, b-j, c-k, d-l)
  subtract (a,b,c,d) (i,j,k,l) = (subtract a i, subtract b j, subtract c k, subtract d l)
  times n (a,b,c,d) = (times n a,times n b, times n c, times n d)

instance (Group a, Group b, Group c, Group d, Group e) => Group (a,b,c,d,e) where
  negate (a,b,c,d,e) = (negate a, negate b, negate c, negate d, negate e)
  (a,b,c,d,e) - (i,j,k,l,m) = (a-i, b-j, c-k, d-l, e-m)
  subtract (a,b,c,d,e) (i,j,k,l,m) = (subtract a i, subtract b j, subtract c k, subtract d l, subtract e m)
  times n (a,b,c,d,e) = (times n a,times n b, times n c, times n d, times n e)

