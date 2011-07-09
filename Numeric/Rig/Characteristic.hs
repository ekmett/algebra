module Numeric.Rig.Characteristic
  ( Characteristic(..)
  , charInt
  , charWord
  , frobenius
  ) where

import Data.Int
import Data.Word
import Data.Proxy
import Numeric.Rig.Class
import Numeric.Ring.Endomorphism
import Numeric.Natural.Internal
import Numeric.Monoid.Multiplicative
import Prelude hiding ((^))

class Rig r => Characteristic r where
  char :: Proxy r -> Natural

-- the frobenius ring endomorphism (assuming the characteristic is prime)
frobenius :: Characteristic r => End r
frobenius = End $ \r -> r `pow` char (ofRing r)

ofRing :: r -> Proxy r
ofRing _ = Proxy

charInt :: (Integral s, Bounded s) => Proxy s -> Natural
charInt p = 2 * fromIntegral (maxBound `asProxyTypeOf` p) + 2

charWord :: (Whole s, Bounded s) => Proxy s -> Natural
charWord p = toNatural (maxBound `asProxyTypeOf` p) + 1

-- | NB: we're using the boolean semiring, not the boolean ring
instance Characteristic Bool where char _ = 0
instance Characteristic Integer where char _ = 0
instance Characteristic Natural where char _ = 0
instance Characteristic Int where char = charInt
instance Characteristic Int8 where char = charInt
instance Characteristic Int16 where char = charInt
instance Characteristic Int32 where char = charInt
instance Characteristic Int64 where char = charInt
instance Characteristic Word where char = charWord
instance Characteristic Word8 where char = charWord
instance Characteristic Word16 where char = charWord
instance Characteristic Word32 where char = charWord
instance Characteristic Word64 where char = charWord
