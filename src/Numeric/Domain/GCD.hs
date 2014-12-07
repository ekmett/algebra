module Numeric.Domain.GCD (GCDDomain(..), gcd') where

import Numeric.Domain.Internal(GCDDomain(..))
import Numeric.Algebra.Unital
import Numeric.Algebra.Unital.UnitNormalForm
import Prelude()

gcd' :: GCDDomain r => [r] -> r
gcd' []     = one
gcd' [x]    = normalize x
gcd' [x,y]  = gcd x y
gcd' (x:xs) = gcd x (gcd' xs)
