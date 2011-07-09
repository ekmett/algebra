module Numeric.Rig.Ordered
  ( OrderedRig
  ) where

import Numeric.Rig.Class
import Numeric.Order.Additive
import Numeric.Natural.Internal

-- x <= y ==> x + z <= y + z
-- 0 <= x && y <= z implies xy <= xz
-- 0 <= x <= 1
class (AdditiveOrder r, Rig r) => OrderedRig r

instance OrderedRig Integer
instance OrderedRig Natural
instance OrderedRig Bool
