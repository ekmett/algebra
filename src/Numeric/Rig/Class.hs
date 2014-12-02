module Numeric.Rig.Class
  ( Rig(..)
  ) where

import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Data.Int
import Data.Word
import Prelude (Integer,Bool,(/=),id,fromIntegral)
import Numeric.Natural

-- | A Ring without (n)egation
class (Semiring r, Unital r, Monoidal r) => Rig r where
  fromNatural :: Natural -> r
  fromNatural n = sinnum n one

instance Rig Integer where fromNatural = fromIntegral
instance Rig Natural where fromNatural = id
instance Rig Bool where fromNatural = (/=) 0
instance Rig Int where fromNatural = fromIntegral
instance Rig Int8 where fromNatural = fromIntegral
instance Rig Int16 where fromNatural = fromIntegral
instance Rig Int32 where fromNatural = fromIntegral
instance Rig Int64 where fromNatural = fromIntegral
instance Rig Word where fromNatural = fromIntegral
instance Rig Word8 where fromNatural = fromIntegral
instance Rig Word16 where fromNatural = fromIntegral
instance Rig Word32 where fromNatural = fromIntegral
instance Rig Word64 where fromNatural = fromIntegral
instance Rig () where fromNatural _ = ()
instance (Rig a, Rig b) => Rig (a, b) where
  fromNatural n = (fromNatural n, fromNatural n)
instance (Rig a, Rig b, Rig c) => Rig (a, b, c) where
  fromNatural n = (fromNatural n, fromNatural n, fromNatural n)
instance (Rig a, Rig b, Rig c, Rig d) => Rig (a, b, c, d) where
  fromNatural n = (fromNatural n, fromNatural n, fromNatural n, fromNatural n)
instance (Rig a, Rig b, Rig c, Rig d, Rig e) => Rig (a, b, c, d, e) where
  fromNatural n = (fromNatural n, fromNatural n, fromNatural n, fromNatural n, fromNatural n)
