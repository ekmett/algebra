module Numeric.Coalgebra.Trigonometric.Class
  ( Trigonometric(..)
  ) where

import Prelude (return)
import Numeric.Covector

class Trigonometric r where
  cos :: r
  sin :: r

instance Trigonometric a => Trigonometric (Covector r a) where
  cos = return cos
  sin = return sin
