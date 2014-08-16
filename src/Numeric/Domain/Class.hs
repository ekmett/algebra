{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Numeric.Domain.Class where
import Numeric.Ring.Class
import Numeric.Semiring.Integral

-- | (Integral) domain is the integral semiring.
class (IntegralSemiring d, Ring d) => Domain d
instance (IntegralSemiring d, Ring d) => Domain d
