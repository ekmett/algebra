module Numeric.Multiplication.Invertible
  ( Invertible(..)
  ) where

import Prelude hiding ((*), recip, (/),(^))
import Numeric.Multiplicative
import Numeric.Multiplication.Unital

infixr 8 ^
infixl 7 /, \\

-- A multiplicative group
class Unital r => Invertible r where
  recip  :: r -> r
  (/)    :: r -> r -> r
  (\\)   :: r -> r -> r
  (^)    :: Integral n => r -> n -> r
  recip a = one / a
  a / b = a * recip b
  a \\ b = recip a * b
  x0 ^ y0 = case compare y0 0 of
    LT -> f (recip x0) (negate y0)
    EQ -> one
    GT -> f x0 y0
    where
       f x y 
         | even y = f (x * x) (y `quot` 2)
         | y == 1 = x
         | otherwise = g (x * x) ((y - 1) `quot` 2) x
       g x y z 
         | even y = g (x * x) (y `quot` 2) z
         | y == 1 = x * z
         | otherwise = g (x * x) ((y - 1) `quot` 2) (x * z)

instance Invertible () where 
  _ / _   = ()
  recip _ = ()
  _ \\ _  = ()
  _ ^ _   = ()

instance (Invertible a, Invertible b) => Invertible (a,b) where
  recip (a,b) = (recip a, recip b)
  (a,b) / (i,j) = (a/i,b/j)
  (a,b) \\ (i,j) = (a\\i,b\\j)
  (a,b) ^ n = (a^n,b^n)

instance (Invertible a, Invertible b, Invertible c) => Invertible (a,b,c) where
  recip (a,b,c) = (recip a, recip b, recip c)
  (a,b,c) / (i,j,k) = (a/i,b/j,c/k)
  (a,b,c) \\ (i,j,k) = (a\\i,b\\j,c\\k)
  (a,b,c) ^ n = (a^n,b^n,c^n)

instance (Invertible a, Invertible b, Invertible c, Invertible d) => Invertible (a,b,c,d) where
  recip (a,b,c,d) = (recip a, recip b, recip c, recip d)
  (a,b,c,d) / (i,j,k,l) = (a/i,b/j,c/k,d/l)
  (a,b,c,d) \\ (i,j,k,l) = (a\\i,b\\j,c\\k,d\\l)
  (a,b,c,d) ^ n = (a^n,b^n,c^n,d^n)

instance (Invertible a, Invertible b, Invertible c, Invertible d, Invertible e) => Invertible (a,b,c,d,e) where
  recip (a,b,c,d,e) = (recip a, recip b, recip c, recip d, recip e)
  (a,b,c,d,e) / (i,j,k,l,m) = (a/i,b/j,c/k,d/l,e/m)
  (a,b,c,d,e) \\ (i,j,k,l,m) = (a\\i,b\\j,c\\k,d\\l,e\\m)
  (a,b,c,d,e) ^ n = (a^n,b^n,c^n,d^n,e^n)
