module Numeric.Domain.GCD (GCDDomain(..), gcd') where

import Numeric.Domain.Internal(GCDDomain(..))
import Numeric.Algebra.Unital
import Prelude()

gcd' :: GCDDomain r => [r] -> r
gcd' []     = one
gcd' [x]    = x
gcd' [x,y]  = gcd x y
gcd' (x:xs) = gcd x (gcd' xs)
