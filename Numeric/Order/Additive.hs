module Numeric.Order.Additive
  ( AdditiveOrder
  ) where

import Numeric.Natural
import Numeric.Additive
import Numeric.Order.Class

-- An additive semigroup with a partial order (<=)

-- | z + x <= z + y = x <= y = x + z <= y + z
class (Additive r, Order r) => AdditiveOrder r

instance AdditiveOrder Integer
instance AdditiveOrder Natural
instance AdditiveOrder Bool
instance AdditiveOrder ()
instance (AdditiveOrder a, AdditiveOrder b) => AdditiveOrder (a,b)
instance (AdditiveOrder a, AdditiveOrder b, AdditiveOrder c) => AdditiveOrder (a,b,c)
instance (AdditiveOrder a, AdditiveOrder b, AdditiveOrder c, AdditiveOrder d) => AdditiveOrder (a,b,c,d)
instance (AdditiveOrder a, AdditiveOrder b, AdditiveOrder c, AdditiveOrder d, AdditiveOrder e) => AdditiveOrder (a,b,c,d,e)
