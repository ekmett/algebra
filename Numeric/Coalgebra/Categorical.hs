{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Numeric.Coalgebra.Categorical 
  ( Morphism(..)
  ) where

import Data.Data
import Numeric.Partial.Semigroup
import Numeric.Partial.Monoid
import Numeric.Partial.Group
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Algebra.Commutative

-- the dual categorical algebra
newtype Morphism a = Morphism a deriving (Eq,Ord,Show,Read,PartialSemigroup,PartialMonoid,PartialGroup,Data,Typeable)

instance (Commutative r, Monoidal r, Semiring r, PartialSemigroup a) => Coalgebra r (Morphism a) where
  comult f a b 
    | Just c <- padd a b = f c
    | otherwise = zero

instance (Commutative r, Monoidal r, Semiring r, PartialMonoid a) => CounitalCoalgebra r (Morphism a) where
  counit f = f pzero
