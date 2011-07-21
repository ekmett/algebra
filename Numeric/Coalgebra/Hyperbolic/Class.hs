module Numeric.Coalgebra.Hyperbolic.Class
  ( Hyperbolic(..)
  ) where

import Prelude (return)
import Numeric.Covector

class Hyperbolic r where
  cosh :: r
  sinh :: r

instance Hyperbolic a => Hyperbolic (Covector r a) where
  cosh = return cosh
  sinh = return sinh
