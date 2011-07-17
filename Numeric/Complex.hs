module Numeric.Complex
  ( ComplexBasis(..)
  , Complex
  ) where

import Numeric.Algebra

-- complex basis
data ComplexBasis = E | I
type Complex = (:->:) C

instance HasTrie ComplexBasis where
  type BaseTrie ComplexBasis = BaseTrie Bool
  embedKey E = embedKey False
  embedKey I = embedKey True
  projectKey t | projectKey t = I 
               | otherwise    = E

instance Group k => Algebra k ComplexBasis where
  mult f E = f E E - f I I
  mult f I = f E I + f I E

instance Group k => UnitalAlgebra k ComplexBasis where
  unit x E = x 
  unit x _ = zero

instance Group k => Coalgebra k ComplexBasis where
  comult f E b = f b
  comult f b E = f b 
  comult f I I = - f E

instance Group k => CounitalCoalgebra k ComplexBasis where
  counit f = f E

instance Group k => Bialgebra k ComplexBasis

instance Group k => InvolutiveAlgebra k ComplexBasis where
  inv f E = f E
  inv f I = - f I

instance Group k => InvolutiveCoalgebra k ComplexBasis where
  coinv f E = f E
  coinv f I = - f I

instance Group k => HopfAlgebra k ComplexBasis where
  antipode = inv
  
