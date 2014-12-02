module Numeric.Semiring.Integral 
  ( IntegralSemiring
  ) where

import Numeric.Algebra.Class
import Numeric.Natural

-- | An integral semiring has no zero divisors
--
-- > a * b = 0 implies a == 0 || b == 0
class (Monoidal r, Semiring r) => IntegralSemiring r

instance IntegralSemiring Integer
instance IntegralSemiring Natural
instance IntegralSemiring Bool
