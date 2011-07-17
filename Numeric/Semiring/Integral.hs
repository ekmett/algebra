module Numeric.Semiring.Integral 
  ( IntegralSemiring
  ) where

import Numeric.Semiring.Class
import Numeric.Addition.Monoidal
import Numeric.Natural.Internal

-- a * b = 0 implies a == 0 || b == 0
class (Monoidal r, Semiring r) => IntegralSemiring r

instance IntegralSemiring Integer
instance IntegralSemiring Natural
instance IntegralSemiring Bool
