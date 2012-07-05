{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances
           , DeriveDataTypeable
           #-}

module Numeric.Coalgebra.Incidence
  ( Interval'(..)
  , zeta'
  , moebius'
  ) where

import Data.Data
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Algebra.Commutative
import Numeric.Ring.Class
import Numeric.Order.LocallyFinite

-- | the dual incidence algebra basis
data Interval' a = Interval' a a deriving (Eq,Ord,Show,Read,Data,Typeable)

instance (Eq a, Commutative r, Monoidal r, Semiring r) => Coalgebra r (Interval' a) where
  comult f (Interval' a b) (Interval' b' c) 
    | b == b' = f (Interval' a c)
    | otherwise = zero

instance (Eq a, Bounded a, Commutative r, Monoidal r, Semiring r) => CounitalCoalgebra r (Interval' a) where
  counit f = f (Interval' minBound maxBound)
  
zeta' :: Unital r => Interval' a -> r
zeta' = const one

moebius' :: (Ring r, LocallyFiniteOrder a) => Interval' a -> r
moebius' (Interval' a b) = moebiusInversion a b
