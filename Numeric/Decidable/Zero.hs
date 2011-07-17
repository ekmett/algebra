module Numeric.Decidable.Zero 
  ( DecidableZero(..)
  ) where

import Numeric.Addition.Monoidal
import Data.Int
import Data.Word
import Numeric.Natural

class Monoidal r => DecidableZero r where
  isZero :: r -> Bool

instance DecidableZero Bool where isZero = not
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

instance DecidableZero () where isZero _ = True

instance (DecidableZero a, DecidableZero b) => DecidableZero (a, b) where
  isZero (a,b) = isZero a && isZero b

instance (DecidableZero a, DecidableZero b, DecidableZero c) => DecidableZero (a, b, c) where
  isZero (a,b,c) = isZero a && isZero b && isZero c

instance (DecidableZero a, DecidableZero b, DecidableZero c, DecidableZero d) => DecidableZero (a, b, c, d) where
  isZero (a,b,c,d) = isZero a && isZero b && isZero c && isZero d

instance (DecidableZero a, DecidableZero b, DecidableZero c, DecidableZero d, DecidableZero e) => DecidableZero (a, b, c, d, e) where
  isZero (a,b,c,d,e) = isZero a && isZero b && isZero c && isZero d && isZero e
