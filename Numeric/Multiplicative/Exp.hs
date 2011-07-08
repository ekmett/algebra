module Numeric.Multiplicative.Exp
  ( Exp(..)
  ) where

import Numeric.Multiplicative.Class
import Numeric.Multiplicative.Monoid.Class
import Numeric.Multiplicative.Group.Class
import Numeric.Multiplicative.Commutative.Class
import Numeric.Additive.Class
import Numeric.Additive.Monoid.Class
import Numeric.Additive.Group.Class
import Numeric.Additive.Abelian.Class

import Prelude hiding ((+),(-),negate,replicate,subtract)

newtype Exp r = Exp { runExp :: r } 

instance Additive r => Multiplicative (Exp r) where
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

instance Abelian r => Commutative (Exp r)
