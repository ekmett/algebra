{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Algebra.Class 
  ( Algebra(..)
  , Coalgebra(..)
  ) where

import Numeric.Semiring.Internal
import Data.Sequence
import Data.Monoid (mappend)
import Prelude ()

-- A coassociative coalgebra over a semiring using
class Semiring r => Coalgebra r c where
  comult :: (c -> r) -> c -> c -> r

-- convolve :: (Algebra r a, Coalgebra r c) => ((c -> r) -> a -> r) -> ((c -> r) -> a -> r) -> ((c -> r) -> a -> r

-- | Every coalgebra gives rise to an algebra by vector space duality classically.
-- Sadly, it requires vector space duality, which we cannot use constructively.
-- This is the dual, which relies in the fact that any constructive coalgebra can only inspect a finite number of coefficients.
instance Algebra r m => Coalgebra r (m -> r) where
  comult k f g = k (f * g)

instance Coalgebra () c where
  comult _ _ _ = ()

instance (Algebra r b, Coalgebra r c) => Coalgebra (b -> r) c where
  comult f c1 c2 b = comult (`f` b) c1 c2 

instance (Coalgebra r a, Coalgebra r b) => Coalgebra r (a, b) where
  comult f (a1,b1) (a2,b2) = comult (\a -> comult (\b -> f (a,b)) b1 b2) a1 a2

instance (Coalgebra r a, Coalgebra r b, Coalgebra r c) => Coalgebra r (a, b, c) where
  comult f (a1,b1,c1) (a2,b2,c2) = comult (\a -> comult (\b -> comult (\c -> f (a,b,c)) c1 c2) b1 b2) a1 a2

instance (Coalgebra r a, Coalgebra r b, Coalgebra r c, Coalgebra r d) => Coalgebra r (a, b, c, d) where
  comult f (a1,b1,c1,d1) (a2,b2,c2,d2) = comult (\a -> comult (\b -> comult (\c -> comult (\d -> f (a,b,c,d)) d1 d2) c1 c2) b1 b2) a1 a2

instance (Coalgebra r a, Coalgebra r b, Coalgebra r c, Coalgebra r d, Coalgebra r e) => Coalgebra r (a, b, c, d, e) where
  comult f (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = comult (\a -> comult (\b -> comult (\c -> comult (\d -> comult (\e -> f (a,b,c,d,e)) e1 e2) d1 d2) c1 c2) b1 b2) a1 a2

instance Semiring r => Coalgebra r [a] where
  comult f as bs = f (mappend as bs)

instance Semiring r => Coalgebra r (Seq a) where
  comult f as bs = f (mappend as bs)

