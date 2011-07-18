{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Numeric.Algebra.Idempotent 
  ( Band
  , pow1pBand
  , powBand
  -- * Idempotent algebras
  , IdempotentAlgebra
  , IdempotentCoalgebra
  , IdempotentBialgebra
  ) where

import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Natural
import Data.Set (Set)
import Data.IntSet (IntSet)

-- | An multiplicative semigroup with idempotent multiplication.
--
-- > a * a = a
class Multiplicative r => Band r

pow1pBand :: Whole n => r -> n -> r
pow1pBand r _ = r 

powBand :: (Unital r, Whole n) => r -> n -> r
powBand _ 0 = one
powBand r _ = r

instance Band ()
instance Band Bool
instance (Band a, Band b) => Band (a,b)
instance (Band a, Band b, Band c) => Band (a,b,c)
instance (Band a, Band b, Band c, Band d) => Band (a,b,c,d)
instance (Band a, Band b, Band c, Band d, Band e) => Band (a,b,c,d,e)

-- idempotent algebra
class Algebra r a => IdempotentAlgebra r a
instance (Semiring r, Band r, Ord a) => IdempotentAlgebra r (Set a)
instance (Semiring r, Band r) => IdempotentAlgebra r IntSet
instance (Semiring r, Band r) => IdempotentAlgebra r ()
instance (IdempotentAlgebra r a, IdempotentAlgebra r b) => IdempotentAlgebra r (a,b)
instance (IdempotentAlgebra r a, IdempotentAlgebra r b, IdempotentAlgebra r c) => IdempotentAlgebra r (a,b,c)
instance (IdempotentAlgebra r a, IdempotentAlgebra r b, IdempotentAlgebra r c, IdempotentAlgebra r d) => IdempotentAlgebra r (a,b,c,d)
instance (IdempotentAlgebra r a, IdempotentAlgebra r b, IdempotentAlgebra r c, IdempotentAlgebra r d, IdempotentAlgebra r e) => IdempotentAlgebra r (a,b,c,d,e)

-- idempotent coalgebra
class Coalgebra r c => IdempotentCoalgebra r c
instance (Semiring r, Band r, Ord c) => IdempotentCoalgebra r (Set c)
instance (Semiring r, Band r) => IdempotentCoalgebra r IntSet
instance (Semiring r, Band r) => IdempotentCoalgebra r ()
instance (IdempotentCoalgebra r a, IdempotentCoalgebra r b) => IdempotentCoalgebra r (a,b)
instance (IdempotentCoalgebra r a, IdempotentCoalgebra r b, IdempotentCoalgebra r c) => IdempotentCoalgebra r (a,b,c)
instance (IdempotentCoalgebra r a, IdempotentCoalgebra r b, IdempotentCoalgebra r c, IdempotentCoalgebra r d) => IdempotentCoalgebra r (a,b,c,d)
instance (IdempotentCoalgebra r a, IdempotentCoalgebra r b, IdempotentCoalgebra r c, IdempotentCoalgebra r d, IdempotentCoalgebra r e) => IdempotentCoalgebra r (a,b,c,d,e)

-- idempotent bialgebra
class (Bialgebra r h, IdempotentAlgebra r h, IdempotentCoalgebra r h) => IdempotentBialgebra r h 
instance (Bialgebra r h, IdempotentAlgebra r h, IdempotentCoalgebra r h) => IdempotentBialgebra r h 
