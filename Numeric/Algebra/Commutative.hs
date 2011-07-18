{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-}
module Numeric.Algebra.Commutative 
  ( CommutativeAlgebra
  , CommutativeCoalgebra
  , CommutativeBialgebra
  ) where

import Numeric.Semiring.Internal
import Numeric.Algebra.Class
import Numeric.Bialgebra
import Numeric.Multiplication.Commutative.Internal
import Prelude ()

class Coalgebra r c => CommutativeCoalgebra r c
instance (Algebra r a, CommutativeCoalgebra r c) => CommutativeCoalgebra (a -> r) c -- TODO: check this instance!
instance CommutativeCoalgebra () a
instance CommutativeAlgebra r m => CommutativeCoalgebra r (m -> r)
instance (CommutativeCoalgebra r a, CommutativeCoalgebra r b) => CommutativeCoalgebra r (a,b)
instance (CommutativeCoalgebra r a, CommutativeCoalgebra r b, CommutativeCoalgebra r c) => CommutativeCoalgebra r (a,b,c)
instance (CommutativeCoalgebra r a, CommutativeCoalgebra r b, CommutativeCoalgebra r c, CommutativeCoalgebra r d) => CommutativeCoalgebra r (a,b,c,d)
instance (CommutativeCoalgebra r a, CommutativeCoalgebra r b, CommutativeCoalgebra r c, CommutativeCoalgebra r d, CommutativeCoalgebra r e) => CommutativeCoalgebra r (a,b,c,d,e)

class    (Bialgebra r h, CommutativeAlgebra r h, CommutativeCoalgebra r h) => CommutativeBialgebra r h
instance (Bialgebra r h, CommutativeAlgebra r h, CommutativeCoalgebra r h) => CommutativeBialgebra r h

-- TODO: add commutative coalgebras for multisets 
-- and a commutative band coalgebra for sets
