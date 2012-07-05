module Numeric.Exp
  ( Exp(..)
  ) where

import Data.Function (on)
import Numeric.Algebra

import Prelude hiding ((+),(-),negate,replicate,subtract)

newtype Exp r = Exp { runExp :: r } 

instance Additive r => Multiplicative (Exp r) where
  Exp a * Exp b = Exp (a + b)
  productWith1 f = Exp . sumWith1 (runExp . f)
  pow1p (Exp m) n = Exp (sinnum1p n m)

instance Monoidal r => Unital (Exp r) where
  one = Exp zero
  pow (Exp m) n = Exp (sinnum n m)
  productWith f = Exp . sumWith (runExp . f)

instance Group r => Division (Exp r) where
  Exp a / Exp b = Exp (a - b)
  recip (Exp a) = Exp (negate a)
  Exp a \\ Exp b = Exp (subtract a b)
  Exp m ^ n = Exp (times n m)

instance Abelian r => Commutative (Exp r)

instance Idempotent r => Band (Exp r)

instance Partitionable r => Factorable (Exp r) where
  factorWith f = partitionWith (f `on` Exp) . runExp
