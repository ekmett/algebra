{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Algebra.Free.Class 
  ( FreeAlgebra(..)
  , FreeCoalgebra(..)
  ) where

import Numeric.Semiring.Internal
import Prelude ()

-- A coassociative coalgebra over a semiring using
class Semiring r => FreeCoalgebra r c where
  cojoin :: (c -> r) -> c -> c -> r

-- convolve :: (FreeAlgebra r a, FreeCoalgebra r c) => ((c -> r) -> a -> r) -> ((c -> r) -> a -> r) -> ((c -> r) -> a -> r

-- | Every coalgebra gives rise to an algebra by vector space duality classically.
-- Sadly, it requires vector space duality, which we cannot use constructively.
-- This is the dual, which relies in the fact that any constructive coalgebra can only inspect a finite number of coefficients.
instance FreeAlgebra r m => FreeCoalgebra r (m -> r) where
  cojoin k f g = k (f * g)

instance FreeCoalgebra () c where
  cojoin _ _ _ = ()

instance (FreeAlgebra r b, FreeCoalgebra r c) => FreeCoalgebra (b -> r) c where
  cojoin f c1 c2 b = cojoin (`f` b) c1 c2 

instance (FreeCoalgebra r a, FreeCoalgebra r b) => FreeCoalgebra r (a, b) where
  cojoin f (a1,b1) (a2,b2) = cojoin (\a -> cojoin (\b -> f (a,b)) b1 b2) a1 a2

instance (FreeCoalgebra r a, FreeCoalgebra r b, FreeCoalgebra r c) => FreeCoalgebra r (a, b, c) where
  cojoin f (a1,b1,c1) (a2,b2,c2) = cojoin (\a -> cojoin (\b -> cojoin (\c -> f (a,b,c)) c1 c2) b1 b2) a1 a2

instance (FreeCoalgebra r a, FreeCoalgebra r b, FreeCoalgebra r c, FreeCoalgebra r d) => FreeCoalgebra r (a, b, c, d) where
  cojoin f (a1,b1,c1,d1) (a2,b2,c2,d2) = cojoin (\a -> cojoin (\b -> cojoin (\c -> cojoin (\d -> f (a,b,c,d)) d1 d2) c1 c2) b1 b2) a1 a2

instance (FreeCoalgebra r a, FreeCoalgebra r b, FreeCoalgebra r c, FreeCoalgebra r d, FreeCoalgebra r e) => FreeCoalgebra r (a, b, c, d, e) where
  cojoin f (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = cojoin (\a -> cojoin (\b -> cojoin (\c -> cojoin (\d -> cojoin (\e -> f (a,b,c,d,e)) e1 e2) d1 d2) c1 c2) b1 b2) a1 a2
