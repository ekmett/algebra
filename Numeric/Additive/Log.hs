module Numeric.Additive.Log 
  ( Log(..)
  ) where

import Numeric.Multiplicative.Semigroup
import Numeric.Multiplicative.Monoid
import Numeric.Multiplicative.Group
import Numeric.Additive.Semigroup
import Numeric.Additive.Monoid
import Numeric.Additive.Group

import Prelude hiding ((*),(^),(/),recip,negate,subtract)

newtype Log r = Log { runLog :: r } 

instance MultiplicativeSemigroup r => AdditiveSemigroup (Log r) where
  Log a + Log b = Log (a * b)
  sumWith1 f = Log . productWith1 (runLog . f)
  replicate n (Log m) = Log (m ^ n)

instance MultiplicativeMonoid r => AdditiveMonoid (Log r) where
  zero = Log one
  sumWith f = Log . productWith (runLog . f)

instance MultiplicativeGroup r => AdditiveGroup (Log r) where
  Log a - Log b = Log (a / b)
  negate (Log a) = Log (recip a)
  subtract (Log a) (Log b) = Log (a \\ b)

instance MultiplicativeAbelianGroup r => AdditiveAbelianGroup (Log r)
