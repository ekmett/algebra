module Numeric.Ring.Local 
  ( LocalRing 
  ) where

import Numeric.Ring.Class

-- forall x in r, either x or 1 - x is a unit.
-- if a finite sum is a unit then so are some of its terms, so the empty sum is not a unit, and one /= zero.
class Ring r => LocalRing r

