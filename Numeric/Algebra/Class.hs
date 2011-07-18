{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Algebra.Class 
  ( Algebra(..)
  , Coalgebra(..)
  ) where

import Numeric.Semiring.Internal
import Numeric.Additive
import Data.Sequence
import Data.Monoid (mappend)
import Data.Set (Set)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Prelude (Ord)

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

-- | The tensor Hopf algebra
instance Semiring r => Coalgebra r [a] where
  comult f as bs = f (mappend as bs)

-- | The tensor Hopf algebra
instance Semiring r => Coalgebra r (Seq a) where
  comult f as bs = f (mappend as bs)

-- | the free commutative band coalgebra
instance (Semiring r, Ord a) => Coalgebra r (Set a) where
  comult f as bs = f (Set.union as bs)

-- | the free commutative band coalgebra over Int
instance Semiring r => Coalgebra r IntSet where
  comult f as bs = f (IntSet.union as bs)

-- | the free commutative coalgebra over a set and a given semigroup
instance (Semiring r, Ord a, Additive b) => Coalgebra r (Map a b) where
  comult f as bs = f (Map.unionWith (+) as bs)

-- | the free commutative coalgebra over a set and Int
instance (Semiring r, Additive b) => Coalgebra r (IntMap b) where
  comult f as bs = f (IntMap.unionWith (+) as bs)
