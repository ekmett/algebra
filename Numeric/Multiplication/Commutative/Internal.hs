{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Numeric.Multiplication.Commutative.Internal 
  ( Commutative
  , CommutativeAlgebra
  ) where

import Data.Int
import Data.Word
import Numeric.Multiplicative
import Numeric.Natural
import Numeric.Algebra.Class

-- | A commutative multiplicative semigroup
class Multiplicative r => Commutative r

instance Commutative () 
instance Commutative Bool
instance Commutative Integer
instance Commutative Int
instance Commutative Int8
instance Commutative Int16
instance Commutative Int32
instance Commutative Int64
instance Commutative Natural
instance Commutative Word
instance Commutative Word8
instance Commutative Word16
instance Commutative Word32
instance Commutative Word64
instance (Commutative a, Commutative b) => Commutative (a,b) 
instance (Commutative a, Commutative b, Commutative c) => Commutative (a,b,c) 
instance (Commutative a, Commutative b, Commutative c, Commutative d) => Commutative (a,b,c,d) 
instance (Commutative a, Commutative b, Commutative c, Commutative d, Commutative e) => Commutative (a,b,c,d,e) 

class Algebra r a => CommutativeAlgebra r a
instance CommutativeAlgebra () a
instance (CommutativeAlgebra r a, CommutativeAlgebra r b) => CommutativeAlgebra r (a,b)
instance (CommutativeAlgebra r a, CommutativeAlgebra r b, CommutativeAlgebra r c) => CommutativeAlgebra r (a,b,c)
instance (CommutativeAlgebra r a, CommutativeAlgebra r b, CommutativeAlgebra r c, CommutativeAlgebra r d) => CommutativeAlgebra r (a,b,c,d)
instance (CommutativeAlgebra r a, CommutativeAlgebra r b, CommutativeAlgebra r c, CommutativeAlgebra r d, CommutativeAlgebra r e) => CommutativeAlgebra r (a,b,c,d,e)
instance (Algebra r a, CommutativeAlgebra r b) => CommutativeAlgebra (a -> r) b
instance CommutativeAlgebra r a => Commutative (a -> r)
