{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Numeric.Ring.Division
  ( DivisionRing
  ) where

import Numeric.Algebra.Division
import Numeric.Ring.Class

class (Division r, Ring r) => DivisionRing r
instance (Division r, Ring r) => DivisionRing r
