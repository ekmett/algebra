module Numeric.Rig.Class
  ( Rig
  ) where

import Numeric.Additive.Monoid.Class
import Numeric.Multiplicative.Monoid.Class
import Numeric.Semiring.Class
import Data.Int
import Data.Word

-- | A Ring without (n)egation

class (Semiring r, AdditiveMonoid r, MultiplicativeMonoid r) => Rig r

instance Rig Integer
instance Rig Bool
instance Rig Int
instance Rig Int8
instance Rig Int16
instance Rig Int32
instance Rig Int64
instance Rig Word
instance Rig Word8
instance Rig Word16
instance Rig Word32
instance Rig Word64
