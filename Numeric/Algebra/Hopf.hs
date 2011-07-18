{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Algebra.Hopf
  ( HopfAlgebra(..)
  ) where

import Numeric.Algebra.Unital

-- | A HopfAlgebra algebra on a semiring, where the module is free.
--
-- When @antipode . antipode = id@ and antipode is an antihomomorphism then we are an InvolutiveBialgebra with @inv = antipode@ as well

class Bialgebra r h => HopfAlgebra r h where
  -- > convolve id antipode = convolve antipode id = unit . counit
  antipode :: (h -> r) -> h -> r

instance (UnitalAlgebra r a, HopfAlgebra r h) => HopfAlgebra (a -> r) h where
  antipode f h a = antipode (`f` a) h

instance HopfAlgebra () h where
  antipode = id

instance (HopfAlgebra r a, HopfAlgebra r b) => HopfAlgebra r (a, b) where
  antipode f (a,b) = antipode (\a' -> antipode (\b' -> f (a',b')) b) a

instance (HopfAlgebra r a, HopfAlgebra r b, HopfAlgebra r c) => HopfAlgebra r (a, b, c) where
  antipode f (a,b,c) = antipode (\a' -> antipode (\b' -> antipode (\c' -> f (a',b',c')) c) b) a

instance (HopfAlgebra r a, HopfAlgebra r b, HopfAlgebra r c, HopfAlgebra r d) => HopfAlgebra r (a, b, c, d) where
  antipode f (a,b,c,d) = antipode (\a' -> antipode (\b' -> antipode (\c' -> antipode (\d' -> f (a',b',c',d')) d) c) b) a

instance (HopfAlgebra r a, HopfAlgebra r b, HopfAlgebra r c, HopfAlgebra r d, HopfAlgebra r e) => HopfAlgebra r (a, b, c, d, e) where
  antipode f (a,b,c,d,e) = antipode (\a' -> antipode (\b' -> antipode (\c' -> antipode (\d' -> antipode (\e' -> f (a',b',c',d',e')) e) d) c) b) a
