{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Bialgebra 
  ( Bialgebra
  ) where

import Data.Sequence (Seq)
import Numeric.Semiring.Class
import Numeric.Algebra.Unital
-- import Numeric.Multiplication.Unital
import Numeric.Addition.Monoidal

-- | A bialgebra is both a unital algebra and counital coalgebra 
-- where the `mult` and `unit` are compatible in some sense with 
-- the `comult` and `counit`. That is to say that 
-- 'mult' and 'unit' are a coalgebra homomorphisms or (equivalently) that 
-- 'comult' and 'counit' are an algebra homomorphisms.

class (UnitalAlgebra r a, CounitalCoalgebra r a) => Bialgebra r a

-- TODO
-- instance (Unital r, Bialgebra r m) => Bialgebra r (m -> r)
instance Bialgebra () c
instance (UnitalAlgebra r b, Bialgebra r c) => Bialgebra (b -> r) c
instance (Bialgebra r a, Bialgebra r b) => Bialgebra r (a, b)
instance (Bialgebra r a, Bialgebra r b, Bialgebra r c) => Bialgebra r (a, b, c)
instance (Bialgebra r a, Bialgebra r b, Bialgebra r c, Bialgebra r d) => Bialgebra r (a, b, c, d)
instance (Bialgebra r a, Bialgebra r b, Bialgebra r c, Bialgebra r d, Bialgebra r e) => Bialgebra r (a, b, c, d, e)

instance (Monoidal r, Semiring r) => Bialgebra r [a]
instance (Monoidal r, Semiring r) => Bialgebra r (Seq a)
