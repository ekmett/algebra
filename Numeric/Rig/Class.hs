module Numeric.Rig.Class
  ( Rig(..)
  , fromNaturalNum
  , fromWhole
  ) where

import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Data.Int
import Data.Word
import Prelude (Integer, Bool, Num(fromInteger),(/=),id,(.))
import Numeric.Natural.Internal

fromNaturalNum :: Num r => Natural -> r
fromNaturalNum (Natural n) = fromInteger n

-- | A Ring without (n)egation
class (Semiring r, Unital r, Monoidal r) => Rig r where
  fromNatural :: Natural -> r
  fromNatural n = replicate n one

fromWhole :: (Whole n, Rig r) => n -> r
fromWhole = fromNatural . toNatural
-- TODO: optimize

instance Rig Integer where fromNatural = fromNaturalNum
instance Rig Natural where fromNatural = id
instance Rig Bool where fromNatural = (/=) 0
instance Rig Int where fromNatural = fromNaturalNum
instance Rig Int8 where fromNatural = fromNaturalNum
instance Rig Int16 where fromNatural = fromNaturalNum
instance Rig Int32 where fromNatural = fromNaturalNum
instance Rig Int64 where fromNatural = fromNaturalNum
instance Rig Word where fromNatural = fromNaturalNum
instance Rig Word8 where fromNatural = fromNaturalNum
instance Rig Word16 where fromNatural = fromNaturalNum
instance Rig Word32 where fromNatural = fromNaturalNum
instance Rig Word64 where fromNatural = fromNaturalNum
instance Rig () where fromNatural _ = ()
instance (Rig a, Rig b) => Rig (a, b) where
  fromNatural n = (fromNatural n, fromNatural n)
instance (Rig a, Rig b, Rig c) => Rig (a, b, c) where
  fromNatural n = (fromNatural n, fromNatural n, fromNatural n)
instance (Rig a, Rig b, Rig c, Rig d) => Rig (a, b, c, d) where
  fromNatural n = (fromNatural n, fromNatural n, fromNatural n, fromNatural n)
instance (Rig a, Rig b, Rig c, Rig d, Rig e) => Rig (a, b, c, d, e) where
  fromNatural n = (fromNatural n, fromNatural n, fromNatural n, fromNatural n, fromNatural n)
