{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- This package is an unfortunate ball of mud forced on me by mutual dependencies
module Numeric.Module.Class
  (  
  -- * Module over semirings
    LeftModule(..)
  , RightModule(..)
  ) where

import Data.Int
import Data.Word
import Numeric.Natural.Internal
import Numeric.Semigroup.Additive
import Numeric.Semiring.Internal
import Prelude hiding ((*))

infixl 7 .*, *.

class (Semiring r, Additive m) => LeftModule r m where
  (.*) :: r -> m -> m

instance LeftModule Natural Bool where 
  0 .* _ = False
  _ .* a = a
instance LeftModule Natural Natural where (.*) = (*)
instance LeftModule Natural Integer where Natural n .* m = n * m
instance LeftModule Integer Integer where (.*) = (*) 
instance LeftModule Natural Int where (.*) = (*) . fromIntegral
instance LeftModule Integer Int where (.*) = (*) . fromInteger
instance LeftModule Natural Int8 where (.*) = (*) . fromIntegral
instance LeftModule Integer Int8 where (.*) = (*) . fromInteger
instance LeftModule Natural Int16 where (.*) = (*) . fromIntegral
instance LeftModule Integer Int16 where (.*) = (*) . fromInteger
instance LeftModule Natural Int32 where (.*) = (*) . fromIntegral
instance LeftModule Integer Int32 where (.*) = (*) . fromInteger
instance LeftModule Natural Int64 where (.*) = (*) . fromIntegral
instance LeftModule Integer Int64 where (.*) = (*) . fromInteger
instance LeftModule Natural Word where (.*) = (*) . fromIntegral
instance LeftModule Integer Word where (.*) = (*) . fromInteger
instance LeftModule Natural Word8 where (.*) = (*) . fromIntegral
instance LeftModule Integer Word8 where (.*) = (*) . fromInteger
instance LeftModule Natural Word16 where (.*) = (*) . fromIntegral
instance LeftModule Integer Word16 where (.*) = (*) . fromInteger
instance LeftModule Natural Word32 where (.*) = (*) . fromIntegral
instance LeftModule Integer Word32 where (.*) = (*) . fromInteger
instance LeftModule Natural Word64 where (.*) = (*) . fromIntegral
instance LeftModule Integer Word64 where (.*) = (*) . fromInteger
instance Semiring r => LeftModule r () where _ .* _ = ()
instance LeftModule r m => LeftModule r (e -> m) where (.*) m f e = m .* f e
instance (LeftModule r a, LeftModule r b) => LeftModule r (a, b) where
  n .* (a, b) = (n .* a, n .* b)
instance (LeftModule r a, LeftModule r b, LeftModule r c) => LeftModule r (a, b, c) where
  n .* (a, b, c) = (n .* a, n .* b, n .* c)
instance (LeftModule r a, LeftModule r b, LeftModule r c, LeftModule r d) => LeftModule r (a, b, c, d) where
  n .* (a, b, c, d) = (n .* a, n .* b, n .* c, n .* d)
instance (LeftModule r a, LeftModule r b, LeftModule r c, LeftModule r d, LeftModule r e) => LeftModule r (a, b, c, d, e) where
  n .* (a, b, c, d, e) = (n .* a, n .* b, n .* c, n .* d, n .* e)

class (Semiring r, Additive m) => RightModule r m where
  (*.) :: m -> r -> m

instance RightModule Natural Bool where 
  _ *. 0 = False
  a *. _ = a
instance RightModule Natural Natural where (*.) = (*)
instance RightModule Natural Integer where n *. Natural m = n * m
instance RightModule Integer Integer where (*.) = (*) 
instance RightModule Natural Int where m *. n = m * fromIntegral n
instance RightModule Integer Int where m *. n = m * fromInteger n
instance RightModule Natural Int8 where m *. n = m * fromIntegral n
instance RightModule Integer Int8 where m *. n = m * fromInteger n
instance RightModule Natural Int16 where m *. n = m * fromIntegral n
instance RightModule Integer Int16 where m *. n = m * fromInteger n
instance RightModule Natural Int32 where m *. n = m * fromIntegral n
instance RightModule Integer Int32 where m *. n = m * fromInteger n
instance RightModule Natural Int64 where m *. n = m * fromIntegral n
instance RightModule Integer Int64 where m *. n = m * fromInteger n
instance RightModule Natural Word where m *. n = m * fromIntegral n
instance RightModule Integer Word where m *. n = m * fromInteger n
instance RightModule Natural Word8 where m *. n = m * fromIntegral n
instance RightModule Integer Word8 where m *. n = m * fromInteger n
instance RightModule Natural Word16 where m *. n = m * fromIntegral n
instance RightModule Integer Word16 where m *. n = m * fromInteger n
instance RightModule Natural Word32 where m *. n = m * fromIntegral n
instance RightModule Integer Word32 where m *. n = m * fromInteger n
instance RightModule Natural Word64 where m *. n = m * fromIntegral n
instance RightModule Integer Word64 where m *. n = m * fromInteger n
instance Semiring r => RightModule r () where _ *. _ = ()
instance RightModule r m => RightModule r (e -> m) where (*.) f m e = f e *. m
instance (RightModule r a, RightModule r b) => RightModule r (a, b) where
  (a, b) *. n = (a *. n, b *. n)
instance (RightModule r a, RightModule r b, RightModule r c) => RightModule r (a, b, c) where
  (a, b, c) *. n = (a *. n, b *. n, c *. n)
instance (RightModule r a, RightModule r b, RightModule r c, RightModule r d) => RightModule r (a, b, c, d) where
  (a, b, c, d) *. n = (a *. n, b *. n, c *. n, d *. n)
instance (RightModule r a, RightModule r b, RightModule r c, RightModule r d, RightModule r e) => RightModule r (a, b, c, d, e) where
  (a, b, c, d, e) *. n = (a *. n, b *. n, c *. n, d *. n, e *. n)

