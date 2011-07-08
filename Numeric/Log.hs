module Numeric.Log 
  ( Log(..)
  ) where

import Numeric.Multiplicative.Class
import Numeric.Multiplicative.Monoid.Class
import Numeric.Multiplicative.Group.Class
import Numeric.Multiplicative.Commutative.Class
import Numeric.Multiplicative.Band.Class
import Numeric.Additive.Class
import Numeric.Additive.Monoid.Class
import Numeric.Additive.Group.Class
import Numeric.Additive.Abelian.Class
import Numeric.Additive.Idempotent.Class

import Prelude hiding ((*),(^),(/),recip,negate,subtract)

newtype Log r = Log { runLog :: r } 

instance Multiplicative r => Additive (Log r) where
  Log a + Log b = Log (a * b)
  sumWith1 f = Log . productWith1 (runLog . f)
  replicate1p n (Log m) = Log (pow1p m n)

instance MultiplicativeMonoid r => AdditiveMonoid (Log r) where
  zero = Log one
  replicate n (Log m) = Log (pow m n)
  sumWith f = Log . productWith (runLog . f)

instance MultiplicativeGroup r => AdditiveGroup (Log r) where
  Log a - Log b = Log (a / b)
  negate (Log a) = Log (recip a)
  subtract (Log a) (Log b) = Log (a \\ b)
  times n (Log m) = Log (m ^ n)

instance Commutative r => Abelian (Log r)

instance Band r => Idempotent (Log r)
