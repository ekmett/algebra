module Numeric.Multiplicative.Exp
  ( Exp(..)
  ) where

import Numeric.Multiplicative.Semigroup
import Numeric.Multiplicative.Monoid
import Numeric.Multiplicative.Group
import Numeric.Additive.Semigroup
import Numeric.Additive.Monoid
import Numeric.Additive.Group

import Prelude hiding ((+),(-),negate,replicate,subtract)

newtype Exp r = Exp { runExp :: r } 

instance AdditiveSemigroup r => MultiplicativeSemigroup (Exp r) where
  Exp a * Exp b = Exp (a + b)
  productWith1 f = Exp . sumWith1 (runExp . f)
  Exp m ^ n = Exp (replicate n m)

instance AdditiveMonoid r => MultiplicativeMonoid (Exp r) where
  one = Exp zero
  productWith f = Exp . sumWith (runExp . f)

instance AdditiveGroup r => MultiplicativeGroup (Exp r) where
  Exp a / Exp b = Exp (a - b)
  recip (Exp a) = Exp (negate a)
  Exp a \\ Exp b = Exp (subtract a b)

instance AdditiveAbelianGroup r => MultiplicativeAbelianGroup (Exp r)
