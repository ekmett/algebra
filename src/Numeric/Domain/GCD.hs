{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.Domain.GCD (GCDDomain(..), gcd') where

import Data.List.NonEmpty
import Numeric.Domain.Internal(GCDDomain(..))
import Numeric.Algebra.Unital.UnitNormalForm

gcd' :: GCDDomain r => NonEmpty r -> r
gcd' (x :| [])    = normalize x
gcd' (x :| [y])  = gcd x y
gcd' (x :| y:ys) = gcd x (gcd' (y:|ys))
