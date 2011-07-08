module Numeric.Decidable.Zero 
  ( DecidableZero(..)
  ) where

import Numeric.Additive.Monoid
import Data.Int
import Data.Word
import Numeric.Natural

class AdditiveMonoid r => DecidableZero r where
  isZero :: r -> Bool

instance DecidableZero Integer where isZero = (0==)
instance DecidableZero Int where isZero = (0==)
instance DecidableZero Int8 where isZero = (0==)
instance DecidableZero Int16 where isZero = (0==)
instance DecidableZero Int32 where isZero = (0==)
instance DecidableZero Int64 where isZero = (0==)

instance DecidableZero Natural where isZero = (0==)
instance DecidableZero Word where isZero = (0==)
instance DecidableZero Word8 where isZero = (0==)
instance DecidableZero Word16 where isZero = (0==)
instance DecidableZero Word32 where isZero = (0==)
instance DecidableZero Word64 where isZero = (0==)
