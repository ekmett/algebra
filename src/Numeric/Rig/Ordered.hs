module Numeric.Rig.Ordered
  ( OrderedRig
  ) where

import Numeric.Rig.Class
import Numeric.Order.Additive
import Numeric.Natural

-- x <= y ==> x + z <= y + z
-- 0 <= x && y <= z implies xy <= xz
-- 0 <= x <= 1
class (AdditiveOrder r, Rig r) => OrderedRig r

instance OrderedRig Integer
instance OrderedRig Natural
instance OrderedRig Bool
instance OrderedRig ()
instance (OrderedRig a, OrderedRig b) => OrderedRig (a, b) 
instance (OrderedRig a, OrderedRig b, OrderedRig c) => OrderedRig (a, b, c) 
instance (OrderedRig a, OrderedRig b, OrderedRig c, OrderedRig d) => OrderedRig (a, b, c, d) 
instance (OrderedRig a, OrderedRig b, OrderedRig c, OrderedRig d, OrderedRig e) => OrderedRig (a, b, c, d, e) 
