{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Numeric.Field.Class 
  ( Field
  ) where

import Numeric.Ring.Division
import Numeric.Algebra.Commutative

class (Commutative r, DivisionRing r) => Field r
instance (Commutative r, DivisionRing r) => Field r
