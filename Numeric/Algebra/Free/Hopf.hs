{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Algebra.Free.Hopf
  ( Hopf(..)
  ) where

import Numeric.Algebra.Free.Unital

-- | A Hopf algebra on a semiring, where the module is free.
--
-- If @antipode . antipode = id@ then we are 'Involutive'

class (FreeUnitalAlgebra r h, FreeCounitalCoalgebra r h) => Hopf r h where
  -- > convolve id antipode = convolve antipode id = unit . counit
  antipode :: (h -> r) -> h -> r

instance (FreeUnitalAlgebra r a, Hopf r h) => Hopf (a -> r) h where
  antipode f h a = antipode (`f` a) h

instance Hopf () h where
  antipode = id

instance (Hopf r a, Hopf r b) => Hopf r (a, b) where
  antipode f (a,b) = antipode (\a' -> antipode (\b' -> f (a',b')) b) a

instance (Hopf r a, Hopf r b, Hopf r c) => Hopf r (a, b, c) where
  antipode f (a,b,c) = antipode (\a' -> antipode (\b' -> antipode (\c' -> f (a',b',c')) c) b) a

instance (Hopf r a, Hopf r b, Hopf r c, Hopf r d) => Hopf r (a, b, c, d) where
  antipode f (a,b,c,d) = antipode (\a' -> antipode (\b' -> antipode (\c' -> antipode (\d' -> f (a',b',c',d')) d) c) b) a

instance (Hopf r a, Hopf r b, Hopf r c, Hopf r d, Hopf r e) => Hopf r (a, b, c, d, e) where
  antipode f (a,b,c,d,e) = antipode (\a' -> antipode (\b' -> antipode (\c' -> antipode (\d' -> antipode (\e' -> f (a',b',c',d',e')) e) d) c) b) a
