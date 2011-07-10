module Numeric.Rng.Class
  ( Rng
  ) where

import Numeric.Group.Additive
import Numeric.Semiring
import Data.Int
import Data.Word

-- | A Ring without an /i/dentity.

class (AdditiveGroup r, Semiring r) => Rng r where
instance Rng Integer
instance Rng Int
instance Rng Int8
instance Rng Int16
instance Rng Int32
instance Rng Int64
instance Rng Word
instance Rng Word8
instance Rng Word16
instance Rng Word32
instance Rng Word64
instance Rng ()
instance (Rng a, Rng b) => Rng (a, b)
instance (Rng a, Rng b, Rng c) => Rng (a, b, c)
instance (Rng a, Rng b, Rng c, Rng d) => Rng (a, b, c, d)
instance (Rng a, Rng b, Rng c, Rng d, Rng e) => Rng (a, b, c, d, e)
