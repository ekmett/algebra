module Numeric.Semiring.Integral 
  ( IntegralSemiring
  ) where

import Numeric.Semiring.Class
import Numeric.Monoid.Additive
import Numeric.Natural.Internal

-- a * b = 0 implies a == 0 || b == 0
class (AdditiveMonoid r, Semiring r) => IntegralSemiring r

instance IntegralSemiring Integer
instance IntegralSemiring Natural
instance IntegralSemiring Bool
