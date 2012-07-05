module Numeric.Algebra.Complex.Class
  ( Complicated(..)
  ) where

import Numeric.Algebra.Distinguished.Class
import Numeric.Covector
import Prelude (return)

class Distinguished r => Complicated r where
  i :: r

instance Complicated a => Complicated (Covector r a) where
  i = return i
