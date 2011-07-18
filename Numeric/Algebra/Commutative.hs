{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-}
module Numeric.Algebra.Commutative 
  ( Commutative
  , CommutativeAlgebra
  , CommutativeCoalgebra
  , CommutativeBialgebra
  ) where

import Numeric.Additive.Class
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Prelude (Bool, Ord, Integer)
import Data.Int
import Data.Word
import Numeric.Natural
import Data.Set (Set)
-- import qualified Data.Set as Set
import Data.IntSet (IntSet)
-- import qualified Data.IntSet as IntSet
import Data.Map (Map)
-- import qualified Data.Map as Map
import Data.IntMap (IntMap)
-- import qualified Data.IntMap as IntMap

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

instance (Commutative r, Semiring r) => CommutativeAlgebra r ()
instance (CommutativeAlgebra r a, CommutativeAlgebra r b) => CommutativeAlgebra r (a,b)
instance (CommutativeAlgebra r a, CommutativeAlgebra r b, CommutativeAlgebra r c) => CommutativeAlgebra r (a,b,c)
instance (CommutativeAlgebra r a, CommutativeAlgebra r b, CommutativeAlgebra r c, CommutativeAlgebra r d) => CommutativeAlgebra r (a,b,c,d)
instance (CommutativeAlgebra r a, CommutativeAlgebra r b, CommutativeAlgebra r c, CommutativeAlgebra r d, CommutativeAlgebra r e) => CommutativeAlgebra r (a,b,c,d,e)

-- incoherent
-- instance (Algebra r a, CommutativeAlgebra r b) => CommutativeAlgebra (a -> r) b
-- instance CommutativeAlgebra () a

instance (Commutative r, Semiring r, Ord a) => CommutativeAlgebra r (Set a)
instance (Commutative r, Semiring r) => CommutativeAlgebra r IntSet
instance (Commutative r, Monoidal r, Semiring r, Ord a, Abelian b, Partitionable b) => CommutativeAlgebra r (Map a b)
instance (Commutative r, Monoidal r, Semiring r, Abelian b, Partitionable b) => CommutativeAlgebra r (IntMap b)

instance CommutativeAlgebra r a => Commutative (a -> r)

class Coalgebra r c => CommutativeCoalgebra r c


instance CommutativeAlgebra r m => CommutativeCoalgebra r (m -> r)
instance (CommutativeCoalgebra r a, CommutativeCoalgebra r b) => CommutativeCoalgebra r (a,b)
instance (CommutativeCoalgebra r a, CommutativeCoalgebra r b, CommutativeCoalgebra r c) => CommutativeCoalgebra r (a,b,c)
instance (CommutativeCoalgebra r a, CommutativeCoalgebra r b, CommutativeCoalgebra r c, CommutativeCoalgebra r d) => CommutativeCoalgebra r (a,b,c,d)
instance (CommutativeCoalgebra r a, CommutativeCoalgebra r b, CommutativeCoalgebra r c, CommutativeCoalgebra r d, CommutativeCoalgebra r e) => CommutativeCoalgebra r (a,b,c,d,e)

instance (Commutative r, Semiring r, Ord a) => CommutativeCoalgebra r (Set a)
instance (Commutative r, Semiring r) => CommutativeCoalgebra r IntSet
instance (Commutative r, Semiring r, Ord a, Abelian b) => CommutativeCoalgebra r (Map a b)
instance (Commutative r, Semiring r, Abelian b) => CommutativeCoalgebra r (IntMap b)

-- incoherent
-- instance (Algebra r a, CommutativeCoalgebra r c) => CommutativeCoalgebra (a -> r) c -- TODO: check this instance!
-- instance CommutativeCoalgebra () a

class    (Bialgebra r h, CommutativeAlgebra r h, CommutativeCoalgebra r h) => CommutativeBialgebra r h
instance (Bialgebra r h, CommutativeAlgebra r h, CommutativeCoalgebra r h) => CommutativeBialgebra r h
