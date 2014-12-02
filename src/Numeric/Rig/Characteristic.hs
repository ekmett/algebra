module Numeric.Rig.Characteristic
  ( Characteristic(..)
  , charInt
  , charWord
  ) where

import Data.Int
import Data.Word
import Numeric.Rig.Class
import Numeric.Natural
import Prelude hiding ((^))

data Proxy p = Proxy

class Rig r => Characteristic r where
  char :: proxy r -> Natural

charInt :: (Integral s, Bounded s) => proxy s -> Natural
charInt p = 2 * fromIntegral (maxBound `asProxyTypeOf` p) + 2

asProxyTypeOf :: a -> p a -> a
asProxyTypeOf = const

charWord :: (Integral s, Bounded s) => proxy s -> Natural
charWord p = fromIntegral (maxBound `asProxyTypeOf` p) + 1

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
instance Characteristic () where char _ = 1

instance (Characteristic a, Characteristic b) => Characteristic (a,b) where
  char p = char (a p) `lcm` char (b p) where
    a :: proxy (a,b) -> Proxy a
    a _ = Proxy
    b :: proxy (a,b) -> Proxy b
    b _ = Proxy

instance (Characteristic a, Characteristic b, Characteristic c) => Characteristic (a,b,c) where
  char p = char (a p) `lcm` char (b p) `lcm` char (c p) where
    a :: proxy (a,b,c) -> Proxy a
    a _ = Proxy
    b :: proxy (a,b,c) -> Proxy b
    b _ = Proxy
    c :: proxy (a,b,c) -> Proxy c
    c _ = Proxy

instance (Characteristic a, Characteristic b, Characteristic c, Characteristic d) => Characteristic (a,b,c,d) where
  char p = char (a p) `lcm` char (b p) `lcm` char (c p) `lcm` char (d p) where
    a :: proxy (a,b,c,d) -> Proxy a
    a _ = Proxy
    b :: proxy (a,b,c,d) -> Proxy b
    b _ = Proxy
    c :: proxy (a,b,c,d) -> Proxy c
    c _ = Proxy
    d :: proxy (a,b,c,d) -> Proxy d
    d _ = Proxy

instance (Characteristic a, Characteristic b, Characteristic c, Characteristic d, Characteristic e) => Characteristic (a,b,c,d,e) where
  char p = char (a p) `lcm` char (b p) `lcm` char (c p) `lcm` char (d p) `lcm` char (e p) where
    a :: proxy (a,b,c,d,e) -> Proxy a
    a _ = Proxy
    b :: proxy (a,b,c,d,e) -> Proxy b
    b _ = Proxy
    c :: proxy (a,b,c,d,e) -> Proxy c
    c _ = Proxy
    d :: proxy (a,b,c,d,e) -> Proxy d
    d _ = Proxy
    e :: proxy (a,b,c,d,e) -> Proxy e
    e _ = Proxy
