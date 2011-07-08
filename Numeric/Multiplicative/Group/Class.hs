module Numeric.Multiplicative.Group.Class 
  ( MultiplicativeGroup(..)
  ) where

import Prelude hiding ((*), recip, (/),(^))
import Numeric.Multiplicative.Class
import Numeric.Multiplicative.Monoid.Class

infixr 8 ^
infixl 7 /, \\

class MultiplicativeMonoid r => MultiplicativeGroup r where
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
