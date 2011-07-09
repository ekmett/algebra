module Numeric.Band.Rectangular 
  ( Rect(..)
  ) where

import Numeric.Semigroup.Multiplicative
import Numeric.Band.Class
import Data.Semigroupoid

-- | a rectangular band is a nowhere commutative semigroup.
-- That is to say, if ab = ba then a = b. From this it follows
-- classically that aa = a and that such a band is isomorphic 
-- to the following structure
data Rect i j = Rect i j deriving (Eq,Ord,Show,Read)

instance Semigroupoid Rect where
  Rect _ i `o` Rect j _ = Rect j i

instance Multiplicative (Rect i j) where
  Rect i _ * Rect _ j = Rect i j

instance Band (Rect i j)
