{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, UndecidableInstances #-}
module Numeric.Complex
  ( ComplexBasis(..)
  , Complex
  ) where

import Numeric.Algebra
import Numeric.Addition
import Numeric.Multiplication.Involutive
import Numeric.Rng.Class
import Numeric.Semiring.Involutive
import Data.Functor.Representable.Trie
import Prelude hiding ((+),(-),negate)

-- complex basis
data ComplexBasis = E | I
-- TODO: make data Complex a = Complex a a
type Complex = (:->:) ComplexBasis

instance HasTrie ComplexBasis where
  type BaseTrie ComplexBasis = BaseTrie Bool
  embedKey E = embedKey False
  embedKey I = embedKey True
  projectKey t | projectKey t = I 
               | otherwise    = E

instance Rng k => Algebra k ComplexBasis where
  mult f E = f E E - f I I
  mult f I = f E I + f I E

instance Rng k => UnitalAlgebra k ComplexBasis where
  unit x E = x 
  unit _ _ = zero

instance Rng k  => Coalgebra k ComplexBasis where
  comult f E b = f b
  comult f b E = f b 
  comult f I I = negate (f E)

instance Rng k => CounitalCoalgebra k ComplexBasis where
  counit f = f E

instance Rng k => Bialgebra k ComplexBasis

instance (Involutive k, Rng k) => InvolutiveAlgebra k ComplexBasis where
  inv f E = adjoint (f E)
  inv f I = negate (f I)

instance (Involutive k, Rng k) => InvolutiveCoalgebra k ComplexBasis where
  coinv f E = adjoint (f E)
  coinv f I = negate (f I)

instance (Involutive k, Rng k) => HopfAlgebra k ComplexBasis where
  antipode = inv
  
