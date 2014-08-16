{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances, TypeOperators #-}
module Numeric.Algebra.Commutative 
  ( Commutative
  , CommutativeAlgebra
  , CocommutativeCoalgebra
  , CommutativeBialgebra
  ) where

import Data.Int
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.Map (Map)
import Data.Word
import Numeric.Additive.Class
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Natural
import Prelude (Bool, Ord, Integer)



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

instance ( Commutative a
         , Commutative b
         ) => Commutative (a,b) 

instance ( Commutative a
         , Commutative b
         , Commutative c
         ) => Commutative (a,b,c) 

instance ( Commutative a
         , Commutative b
         , Commutative c
         , Commutative d
         ) => Commutative (a,b,c,d) 

instance ( Commutative a
         , Commutative b
         , Commutative c
         , Commutative d
         , Commutative e
         ) => Commutative (a,b,c,d,e)

instance CommutativeAlgebra r a => Commutative (a -> r)

class Algebra r a => CommutativeAlgebra r a

instance ( Commutative r
         , Semiring r
         ) => CommutativeAlgebra r ()

instance ( CommutativeAlgebra r a
         , CommutativeAlgebra r b
         ) => CommutativeAlgebra r (a,b)

instance ( CommutativeAlgebra r a
         , CommutativeAlgebra r b
         , CommutativeAlgebra r c
         ) => CommutativeAlgebra r (a,b,c)

instance ( CommutativeAlgebra r a
         , CommutativeAlgebra r b
         , CommutativeAlgebra r c
         , CommutativeAlgebra r d
         ) => CommutativeAlgebra r (a,b,c,d)

instance ( CommutativeAlgebra r a
         , CommutativeAlgebra r b
         , CommutativeAlgebra r c
         , CommutativeAlgebra r d
         , CommutativeAlgebra r e
         ) => CommutativeAlgebra r (a,b,c,d,e)

instance ( Commutative r
         , Semiring r
         , Ord a
         ) => CommutativeAlgebra r (Set a)

instance (Commutative r
         , Semiring r
         ) => CommutativeAlgebra r IntSet

instance (Commutative r
         , Monoidal r
         , Semiring r
         , Ord a
         , Abelian b
         , Partitionable b
         ) => CommutativeAlgebra r (Map a b)

instance ( Commutative r
         , Monoidal r
         , Semiring r
         , Abelian b
         , Partitionable b
         ) => CommutativeAlgebra r (IntMap b)



class Coalgebra r c => CocommutativeCoalgebra r c

instance CommutativeAlgebra r m => CocommutativeCoalgebra r (m -> r)

instance (Commutative r, Semiring r) => CocommutativeCoalgebra r ()

instance ( CocommutativeCoalgebra r a
         , CocommutativeCoalgebra r b
         ) => CocommutativeCoalgebra r (a,b)

instance ( CocommutativeCoalgebra r a
         , CocommutativeCoalgebra r b
         , CocommutativeCoalgebra r c
         ) => CocommutativeCoalgebra r (a,b,c)

instance ( CocommutativeCoalgebra r a
         , CocommutativeCoalgebra r b
         , CocommutativeCoalgebra r c
         , CocommutativeCoalgebra r d
         ) => CocommutativeCoalgebra r (a,b,c,d)

instance ( CocommutativeCoalgebra r a
         , CocommutativeCoalgebra r b
         , CocommutativeCoalgebra r c
         , CocommutativeCoalgebra r d
         , CocommutativeCoalgebra r e
         ) => CocommutativeCoalgebra r (a,b,c,d,e)

instance ( Commutative r
         , Semiring r
         , Ord a) => CocommutativeCoalgebra r (Set a)

instance ( Commutative r
         , Semiring r
         ) => CocommutativeCoalgebra r IntSet

instance ( Commutative r
         , Semiring r
         , Ord a
         , Abelian b
         ) => CocommutativeCoalgebra r (Map a b)

instance ( Commutative r
         , Semiring r
         , Abelian b
         ) => CocommutativeCoalgebra r (IntMap b)



class ( Bialgebra r h
      , CommutativeAlgebra r h
      , CocommutativeCoalgebra r h
      ) => CommutativeBialgebra r h

instance ( Bialgebra r h
         , CommutativeAlgebra r h
         , CocommutativeCoalgebra r h
         ) => CommutativeBialgebra r h
