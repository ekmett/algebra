module Numeric.Additive.Log 
  ( Log(..)
  ) where

import Numeric.Multiplicative.Class
import Numeric.Multiplicative.Monoid.Class
import Numeric.Multiplicative.Group.Class
import Numeric.Multiplicative.Commutative.Class
import Numeric.Additive.Class
import Numeric.Additive.Monoid.Class
import Numeric.Additive.Group.Class
import Numeric.Additive.Abelian.Class

import Prelude hiding ((*),(^),(/),recip,negate,subtract)

newtype Log r = Log { runLog :: r } 

instance Multiplicative r => Additive (Log r) where
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

instance Commutative r => Abelian (Log r)
