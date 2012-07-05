{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Quadrance.Class
  ( Quadrance(..)
  ) where

import Data.Int
import Data.Word
import Numeric.Additive.Class
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Rig.Class
import Numeric.Natural.Internal
import Prelude hiding ((+),(*))

-- a module with a computable squared norm
class Additive r => Quadrance r m where
  quadrance :: m -> r

instance Quadrance () a where 
  quadrance _ = ()

instance Monoidal r => Quadrance r () where
  quadrance _ = zero

instance (Quadrance r a, Quadrance r b) => Quadrance r (a,b) where
  quadrance (a,b) = quadrance a + quadrance b

instance (Quadrance r a, Quadrance r b, Quadrance r c) => Quadrance r (a,b,c) where
  quadrance (a,b,c) = quadrance a + quadrance b + quadrance c

instance (Quadrance r a, Quadrance r b, Quadrance r c, Quadrance r d) => Quadrance r (a,b,c,d) where
  quadrance (a,b,c,d) = quadrance a + quadrance b + quadrance c + quadrance d

instance (Quadrance r a, Quadrance r b, Quadrance r c, Quadrance r d, Quadrance r e) => Quadrance r (a,b,c,d,e) where
  quadrance (a,b,c,d,e) = quadrance a + quadrance b + quadrance c + quadrance d + quadrance e

instance Rig r => Quadrance r Bool where
  quadrance False = zero
  quadrance True  = one

sq :: Multiplicative r => r -> r
sq r = r * r

instance Rig r => Quadrance r Int where
  quadrance = fromNatural . Natural . sq . toInteger

instance Rig r => Quadrance r Word where
  quadrance = fromNatural . Natural . sq . toInteger

instance Rig r => Quadrance r Natural where
  quadrance = fromNatural . Natural . sq . toInteger

instance Rig r => Quadrance r Integer where 
  quadrance = fromNatural . Natural . fromInteger . sq

instance Rig r => Quadrance r Int8 where 
  quadrance = fromNatural . Natural . sq . toInteger

instance Rig r => Quadrance r Int16 where 
  quadrance = fromNatural . Natural . sq . toInteger

instance Rig r => Quadrance r Int32 where
  quadrance = fromNatural . Natural . sq . toInteger

instance Rig r => Quadrance r Int64 where
  quadrance = fromNatural . Natural . sq . toInteger

instance Rig r => Quadrance r Word8 where 
  quadrance = fromNatural . Natural . sq . toInteger

instance Rig r => Quadrance r Word16 where 
  quadrance = fromNatural . Natural . sq . toInteger

instance Rig r => Quadrance r Word32 where
  quadrance = fromNatural . Natural . sq . toInteger

instance Rig r => Quadrance r Word64 where
  quadrance = fromNatural . Natural . sq . toInteger

{-
instance InvolutiveSemiring r => Quadrance r (Complex r) where
  quadrance n = e (adjoint n * n)

instance InvolutiveSemiring r => Quadrance r (Quaternion r) where
  quadrance n = e (adjoint n * n)
-}
