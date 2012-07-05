module Numeric.Algebra.Dual.Class
  ( Infinitesimal(..)
  ) where

import Numeric.Algebra.Distinguished.Class
import Numeric.Covector

class Distinguished t => Infinitesimal t where
  d :: t

instance Infinitesimal a => Infinitesimal (Covector r a) where
  d = return d
