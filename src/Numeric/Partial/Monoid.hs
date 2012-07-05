module Numeric.Partial.Monoid
  ( PartialMonoid(..)
  ) where

import Numeric.Partial.Semigroup
import Data.Int
import Data.Word
import Numeric.Natural.Internal

class PartialSemigroup a => PartialMonoid a where
  pzero :: a

instance PartialMonoid Bool where
  pzero = False

instance PartialMonoid Int where
  pzero = 0

instance PartialMonoid Integer where
  pzero = 0

instance PartialMonoid Natural where
  pzero = 0

instance PartialMonoid Int8 where
  pzero = 0

instance PartialMonoid Int16 where
  pzero = 0

instance PartialMonoid Int32 where
  pzero = 0

instance PartialMonoid Int64 where
  pzero = 0

instance PartialMonoid Word where
  pzero = 0

instance PartialMonoid Word8 where
  pzero = 0

instance PartialMonoid Word16 where
  pzero = 0

instance PartialMonoid Word32 where
  pzero = 0

instance PartialMonoid Word64 where
  pzero = 0

instance PartialMonoid () where
  pzero = () 

instance PartialSemigroup a => PartialMonoid (Maybe a) where
  pzero = Nothing

instance (PartialMonoid a, PartialMonoid b) => PartialMonoid (a, b) where
  pzero = (pzero, pzero)

instance (PartialMonoid a, PartialMonoid b, PartialMonoid c) => PartialMonoid (a, b, c) where
  pzero = (pzero, pzero, pzero)

instance (PartialMonoid a, PartialMonoid b, PartialMonoid c, PartialMonoid d) => PartialMonoid (a, b, c, d) where
  pzero = (pzero, pzero, pzero, pzero)

instance (PartialMonoid a, PartialMonoid b, PartialMonoid c, PartialMonoid d, PartialMonoid e) => PartialMonoid (a, b, c, d, e) where
  pzero = (pzero, pzero, pzero, pzero, pzero)
