{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeOperators #-}
module Numeric.Algebra.Class 
  (
  -- * Multiplicative Semigroups
    Multiplicative(..)
  , pow1pIntegral
  , product1
  -- * Semirings
  , Semiring
  -- * Left and Right Modules
  , LeftModule(..)
  , RightModule(..)
  , Module
  -- * Additive Monoids
  , Monoidal(..)
  , sum
  , replicateIdempotent
  -- * Associative algebras
  , Algebra(..)
  -- * Coassociative coalgebras
  , Coalgebra(..)
  ) where

import Control.Applicative
import Data.Foldable hiding (sum, concat)
import Data.Functor.Representable
import Data.Functor.Representable.Trie
import Data.Int
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Key hiding (sum)
import Data.Map (Map)
import Data.Monoid (mappend)
-- import Data.Semigroup.Foldable
import Data.Sequence hiding (reverse,replicate,index)
import Data.Set (Set)
import Data.Word
import Numeric.Additive.Class
import Numeric.Natural.Internal
import Prelude hiding ((*), (+), negate, subtract,(-), recip, (/), foldr, sum, product, replicate, concat)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Prelude

infixr 8 `pow1p`
infixl 7 *, .*, *.

-- | A multiplicative semigroup
class Multiplicative r where
  (*) :: r -> r -> r 

-- class Multiplicative r => PowerAssociative r where
  -- pow1p x n = pow x (1 + n)
  pow1p :: Whole n => r -> n -> r
  pow1p x0 y0 = f x0 (y0 Prelude.+ 1) where
    f x y 
      | even y = f (x * x) (y `quot` 2)
      | y == 1 = x
      | otherwise = g (x * x) ((y Prelude.- 1) `quot` 2) x
    g x y z 
      | even y = g (x * x) (y `quot` 2) z
      | y == 1 = x * z
      | otherwise = g (x * x) ((y Prelude.- 1) `quot` 2) (x * z)

-- class PowerAssociative r => Assocative r where
  productWith1 :: Foldable1 f => (a -> r) -> f a -> r
  productWith1 f = maybe (error "Numeric.Multiplicative.Semigroup.productWith1: empty structure") id . foldl' mf Nothing
    where 
      mf Nothing y = Just $! f y
      mf (Just x) y = Just $! x * f y

product1 :: (Foldable1 f, Multiplicative r) => f r -> r
product1 = productWith1 id

pow1pIntegral :: (Integral r, Integral n) => r -> n -> r
pow1pIntegral r n = r ^ (1 Prelude.+ n)

instance Multiplicative Bool where
  (*) = (&&)
  pow1p m _ = m

instance Multiplicative Natural where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Integer where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Int where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Int8 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Int16 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Int32 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Int64 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Word where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Word8 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Word16 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Word32 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Word64 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative () where
  _ * _ = ()
  pow1p _ _ = ()

instance (Multiplicative a, Multiplicative b) => Multiplicative (a,b) where
  (a,b) * (c,d) = (a * c, b * d)

instance (Multiplicative a, Multiplicative b, Multiplicative c) => Multiplicative (a,b,c) where
  (a,b,c) * (i,j,k) = (a * i, b * j, c * k)

instance (Multiplicative a, Multiplicative b, Multiplicative c, Multiplicative d) => Multiplicative (a,b,c,d) where
  (a,b,c,d) * (i,j,k,l) = (a * i, b * j, c * k, d * l)

instance (Multiplicative a, Multiplicative b, Multiplicative c, Multiplicative d, Multiplicative e) => Multiplicative (a,b,c,d,e) where
  (a,b,c,d,e) * (i,j,k,l,m) = (a * i, b * j, c * k, d * l, e * m)

instance Algebra r a => Multiplicative (a -> r) where
  f * g = mult $ \a b -> f a * g b
instance (HasTrie a, Algebra r a) => Multiplicative (a :->: r) where
  f * g = tabulate $ mult $ \a b -> index f a * index g b

-- | A pair of an additive abelian semigroup, and a multiplicative semigroup, with the distributive laws:
-- 
-- > a(b + c) = ab + ac -- left distribution (we are a LeftNearSemiring)
-- > (a + b)c = ac + bc -- right distribution (we are a [Right]NearSemiring)
--
-- Common notation includes the laws for additive and multiplicative identity in semiring.
--
-- If you want that, look at 'Rig' instead.
--
-- Ideally we'd use the cyclic definition:
--
-- > class (LeftModule r r, RightModule r r, Additive r, Abelian r, Multiplicative r) => Semiring r
--
-- to enforce that every semiring r is an r-module over itself, but Haskell doesn't like that.
class (Additive r, Abelian r, Multiplicative r) => Semiring r
instance Semiring Integer
instance Semiring Natural
instance Semiring Bool
instance Semiring Int
instance Semiring Int8
instance Semiring Int16
instance Semiring Int32
instance Semiring Int64
instance Semiring Word
instance Semiring Word8
instance Semiring Word16
instance Semiring Word32
instance Semiring Word64
instance Semiring ()
instance (Semiring a, Semiring b) => Semiring (a, b)
instance (Semiring a, Semiring b, Semiring c) => Semiring (a, b, c)
instance (Semiring a, Semiring b, Semiring c, Semiring d) => Semiring (a, b, c, d)
instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e) => Semiring (a, b, c, d, e)
instance Algebra r a => Semiring (a -> r) 
instance (HasTrie a, Algebra r a) => Semiring (a :->: r) 

-- | An associative algebra built with a free module over a semiring
class Semiring r => Algebra r a where
  mult :: (a -> a -> r) -> a -> r

instance Algebra () a where
  mult _ _ = ()

-- | The tensor algebra
instance Semiring r => Algebra r [a] where
  mult f = go [] where
    go ls rrs@(r:rs) = f (reverse ls) rrs + go (r:ls) rs
    go ls [] = f (reverse ls) []

-- | The tensor algebra
instance Semiring r => Algebra r (Seq a) where
  mult f = go Seq.empty where
    go ls s = case viewl s of
       EmptyL -> f ls s 
       r :< rs -> f ls s + go (ls |> r) rs

instance Semiring r => Algebra r () where
  mult f = f ()

instance (Semiring r, Ord a) => Algebra r (Set a) where
  mult f = go Set.empty where
    go ls s = case Set.minView s of
       Nothing -> f ls s
       Just (r, rs) -> f ls s + go (Set.insert r ls) rs
instance Semiring r => Algebra r IntSet where
  mult f = go IntSet.empty where
    go ls s = case IntSet.minView s of
       Nothing -> f ls s
       Just (r, rs) -> f ls s + go (IntSet.insert r ls) rs

instance (Semiring r, Monoidal r, Ord a, Partitionable b) => Algebra r (Map a b) -- where
--  mult f xs = case minViewWithKey xs of
--    Nothing -> zero 
--    Just ((k, r), rs) -> ...
instance (Semiring r, Monoidal r, Partitionable a) => Algebra r (IntMap a)

instance (Algebra r a, Algebra r b) => Algebra r (a,b) where
  mult f (a,b) = mult (\a1 a2 -> mult (\b1 b2 -> f (a1,b1) (a2,b2)) b) a

instance (Algebra r a, Algebra r b, Algebra r c) => Algebra r (a,b,c) where
  mult f (a,b,c) = mult (\a1 a2 -> mult (\b1 b2 -> mult (\c1 c2 -> f (a1,b1,c1) (a2,b2,c2)) c) b) a

instance (Algebra r a, Algebra r b, Algebra r c, Algebra r d) => Algebra r (a,b,c,d) where
  mult f (a,b,c,d) = mult (\a1 a2 -> mult (\b1 b2 -> mult (\c1 c2 -> mult (\d1 d2 -> f (a1,b1,c1,d1) (a2,b2,c2,d2)) d) c) b) a

instance (Algebra r a, Algebra r b, Algebra r c, Algebra r d, Algebra r e) => Algebra r (a,b,c,d,e) where
  mult f (a,b,c,d,e) = mult (\a1 a2 -> mult (\b1 b2 -> mult (\c1 c2 -> mult (\d1 d2 -> mult (\e1 e2 -> f (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2)) e) d) c) b) a

-- incoherent
-- instance (Algebra r b, Algebra r a) => Algebra (b -> r) a where mult f a b = mult (\a1 a2 -> f a1 a2 b) a

-- A coassociative coalgebra over a semiring using
class Semiring r => Coalgebra r c where
  comult :: (c -> r) -> c -> c -> r

-- | Every coalgebra gives rise to an algebra by vector space duality classically.
-- Sadly, it requires vector space duality, which we cannot use constructively.
-- The dual argument only relies in the fact that any constructive coalgebra can only inspect a finite number of coefficients, 
-- which we CAN exploit.
instance Algebra r m => Coalgebra r (m -> r) where
  comult k f g = k (f * g)

instance (HasTrie m, Algebra r m) => Coalgebra r (m :->: r) where
  comult k f g = k (f * g)

-- instance Coalgebra () c where comult _ _ _ = ()
-- instance (Algebra r b, Coalgebra r c) => Coalgebra (b -> r) c where comult f c1 c2 b = comult (`f` b) c1 c2 

instance Semiring r => Coalgebra r () where
  comult = const

instance (Coalgebra r a, Coalgebra r b) => Coalgebra r (a, b) where
  comult f (a1,b1) (a2,b2) = comult (\a -> comult (\b -> f (a,b)) b1 b2) a1 a2

instance (Coalgebra r a, Coalgebra r b, Coalgebra r c) => Coalgebra r (a, b, c) where
  comult f (a1,b1,c1) (a2,b2,c2) = comult (\a -> comult (\b -> comult (\c -> f (a,b,c)) c1 c2) b1 b2) a1 a2

instance (Coalgebra r a, Coalgebra r b, Coalgebra r c, Coalgebra r d) => Coalgebra r (a, b, c, d) where
  comult f (a1,b1,c1,d1) (a2,b2,c2,d2) = comult (\a -> comult (\b -> comult (\c -> comult (\d -> f (a,b,c,d)) d1 d2) c1 c2) b1 b2) a1 a2

instance (Coalgebra r a, Coalgebra r b, Coalgebra r c, Coalgebra r d, Coalgebra r e) => Coalgebra r (a, b, c, d, e) where
  comult f (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = comult (\a -> comult (\b -> comult (\c -> comult (\d -> comult (\e -> f (a,b,c,d,e)) e1 e2) d1 d2) c1 c2) b1 b2) a1 a2

-- | The tensor Hopf algebra
instance Semiring r => Coalgebra r [a] where
  comult f as bs = f (mappend as bs)

-- | The tensor Hopf algebra
instance Semiring r => Coalgebra r (Seq a) where
  comult f as bs = f (mappend as bs)

-- | the free commutative band coalgebra
instance (Semiring r, Ord a) => Coalgebra r (Set a) where
  comult f as bs = f (Set.union as bs)

-- | the free commutative band coalgebra over Int
instance Semiring r => Coalgebra r IntSet where
  comult f as bs = f (IntSet.union as bs)

-- | the free commutative coalgebra over a set and a given semigroup
instance (Semiring r, Ord a, Additive b) => Coalgebra r (Map a b) where
  comult f as bs = f (Map.unionWith (+) as bs)

-- | the free commutative coalgebra over a set and Int
instance (Semiring r, Additive b) => Coalgebra r (IntMap b) where
  comult f as bs = f (IntMap.unionWith (+) as bs)

class (Semiring r, Additive m) => LeftModule r m where
  (.*) :: r -> m -> m

instance LeftModule Natural Bool where 
  0 .* _ = False
  _ .* a = a

instance LeftModule Natural Natural where 
  (.*) = (*)

instance LeftModule Natural Integer where 
  Natural n .* m = n * m

instance LeftModule Integer Integer where 
  (.*) = (*) 

instance LeftModule Natural Int where
  (.*) = (*) . fromIntegral

instance LeftModule Integer Int where
  (.*) = (*) . fromInteger

instance LeftModule Natural Int8 where
  (.*) = (*) . fromIntegral

instance LeftModule Integer Int8 where
  (.*) = (*) . fromInteger

instance LeftModule Natural Int16 where
  (.*) = (*) . fromIntegral

instance LeftModule Integer Int16 where
  (.*) = (*) . fromInteger

instance LeftModule Natural Int32 where
  (.*) = (*) . fromIntegral

instance LeftModule Integer Int32 where
  (.*) = (*) . fromInteger

instance LeftModule Natural Int64 where
  (.*) = (*) . fromIntegral

instance LeftModule Integer Int64 where
  (.*) = (*) . fromInteger

instance LeftModule Natural Word where
  (.*) = (*) . fromIntegral

instance LeftModule Integer Word where
  (.*) = (*) . fromInteger

instance LeftModule Natural Word8 where
  (.*) = (*) . fromIntegral

instance LeftModule Integer Word8 where
  (.*) = (*) . fromInteger

instance LeftModule Natural Word16 where
  (.*) = (*) . fromIntegral

instance LeftModule Integer Word16 where
  (.*) = (*) . fromInteger

instance LeftModule Natural Word32 where
  (.*) = (*) . fromIntegral

instance LeftModule Integer Word32 where
  (.*) = (*) . fromInteger

instance LeftModule Natural Word64 where
  (.*) = (*) . fromIntegral

instance LeftModule Integer Word64 where
  (.*) = (*) . fromInteger

instance Semiring r => LeftModule r () where 
  _ .* _ = ()

instance LeftModule r m => LeftModule r (e -> m) where 
  (.*) m f e = m .* f e

instance (HasTrie e, LeftModule r m) => LeftModule r (e :->: m) where 
  (.*) m f = tabulate $ \e -> m .* index f e

instance Additive m => LeftModule () m where 
  _ .* a = a

instance (LeftModule r a, LeftModule r b) => LeftModule r (a, b) where
  n .* (a, b) = (n .* a, n .* b)

instance (LeftModule r a, LeftModule r b, LeftModule r c) => LeftModule r (a, b, c) where
  n .* (a, b, c) = (n .* a, n .* b, n .* c)

instance (LeftModule r a, LeftModule r b, LeftModule r c, LeftModule r d) => LeftModule r (a, b, c, d) where
  n .* (a, b, c, d) = (n .* a, n .* b, n .* c, n .* d)

instance (LeftModule r a, LeftModule r b, LeftModule r c, LeftModule r d, LeftModule r e) => LeftModule r (a, b, c, d, e) where
  n .* (a, b, c, d, e) = (n .* a, n .* b, n .* c, n .* d, n .* e)



class (Semiring r, Additive m) => RightModule r m where
  (*.) :: m -> r -> m

instance RightModule Natural Bool where 
  _ *. 0 = False
  a *. _ = a

instance RightModule Natural Natural where (*.) = (*)

instance RightModule Natural Integer where n *. Natural m = n * m

instance RightModule Integer Integer where (*.) = (*) 

instance RightModule Natural Int where m *. n = m * fromIntegral n

instance RightModule Integer Int where m *. n = m * fromInteger n

instance RightModule Natural Int8 where m *. n = m * fromIntegral n

instance RightModule Integer Int8 where m *. n = m * fromInteger n

instance RightModule Natural Int16 where m *. n = m * fromIntegral n

instance RightModule Integer Int16 where m *. n = m * fromInteger n

instance RightModule Natural Int32 where m *. n = m * fromIntegral n

instance RightModule Integer Int32 where m *. n = m * fromInteger n

instance RightModule Natural Int64 where m *. n = m * fromIntegral n

instance RightModule Integer Int64 where m *. n = m * fromInteger n

instance RightModule Natural Word where m *. n = m * fromIntegral n

instance RightModule Integer Word where m *. n = m * fromInteger n

instance RightModule Natural Word8 where m *. n = m * fromIntegral n

instance RightModule Integer Word8 where m *. n = m * fromInteger n

instance RightModule Natural Word16 where m *. n = m * fromIntegral n

instance RightModule Integer Word16 where m *. n = m * fromInteger n

instance RightModule Natural Word32 where m *. n = m * fromIntegral n

instance RightModule Integer Word32 where m *. n = m * fromInteger n

instance RightModule Natural Word64 where m *. n = m * fromIntegral n

instance RightModule Integer Word64 where m *. n = m * fromInteger n

instance Semiring r => RightModule r () where 
  _ *. _ = ()

instance RightModule r m => RightModule r (e -> m) where 
  (*.) f m e = f e *. m

instance (HasTrie e, RightModule r m) => RightModule r (e :->: m) where 
  (*.) f m = tabulate $ \e -> index f e *. m

instance Additive m => RightModule () m where 
  (*.) = const

instance (RightModule r a, RightModule r b) => RightModule r (a, b) where
  (a, b) *. n = (a *. n, b *. n)

instance (RightModule r a, RightModule r b, RightModule r c) => RightModule r (a, b, c) where
  (a, b, c) *. n = (a *. n, b *. n, c *. n)

instance (RightModule r a, RightModule r b, RightModule r c, RightModule r d) => RightModule r (a, b, c, d) where
  (a, b, c, d) *. n = (a *. n, b *. n, c *. n, d *. n)

instance (RightModule r a, RightModule r b, RightModule r c, RightModule r d, RightModule r e) => RightModule r (a, b, c, d, e) where
  (a, b, c, d, e) *. n = (a *. n, b *. n, c *. n, d *. n, e *. n)



class (LeftModule r m, RightModule r m) => Module r m
instance (LeftModule r m, RightModule r m) => Module r m



-- | An additive monoid
--
-- > zero + a = a = a + zero
class (LeftModule Natural m, RightModule Natural m) => Monoidal m where
  zero :: m

  replicate :: Whole n => n -> m -> m
  replicate 0 _  = zero
  replicate n x0 = f x0 n
    where
      f x y
        | even y = f (x + x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x + x) (unsafePred y `quot` 2) x
      g x y z
        | even y = g (x + x) (y `quot` 2) z
        | y == 1 = x + z
        | otherwise = g (x + x) (unsafePred y `quot` 2) (x + z)

  sumWith :: Foldable f => (a -> m) -> f a -> m
  sumWith f = foldl' (\b a -> b + f a) zero

sum :: (Foldable f, Monoidal m) => f m -> m
sum = sumWith id

replicateIdempotent :: (Integral n, Idempotent r, Monoidal r) => n -> r -> r
replicateIdempotent 0 _ = zero
replicateIdempotent _ x = x

instance Monoidal Bool where 
  zero = False
  replicate 0 _ = False
  replicate _ r = r

instance Monoidal Natural where
  zero = 0
  replicate n r = toNatural n * r

instance Monoidal Integer where 
  zero = 0
  replicate n r = toInteger n * r

instance Monoidal Int where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Int8 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Int16 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Int32 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Int64 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Word where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Word8 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Word16 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Word32 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal Word64 where 
  zero = 0
  replicate n r = fromIntegral n * r

instance Monoidal r => Monoidal (e -> r) where
  zero = const zero
  sumWith f xs e = sumWith (`f` e) xs
  replicate n r e = replicate n (r e)

instance (HasTrie e, Monoidal r) => Monoidal (e :->: r) where
  zero = pure zero
  sumWith f xs = tabulate $ \e -> sumWith (\a -> index (f a) e) xs
  replicate n r = tabulate $ replicate n . index r

instance Monoidal () where 
  zero = ()
  replicate _ () = ()
  sumWith _ _ = ()

instance (Monoidal a, Monoidal b) => Monoidal (a,b) where
  zero = (zero,zero)
  replicate n (a,b) = (replicate n a, replicate n b)

instance (Monoidal a, Monoidal b, Monoidal c) => Monoidal (a,b,c) where
  zero = (zero,zero,zero)
  replicate n (a,b,c) = (replicate n a, replicate n b, replicate n c)

instance (Monoidal a, Monoidal b, Monoidal c, Monoidal d) => Monoidal (a,b,c,d) where
  zero = (zero,zero,zero,zero)
  replicate n (a,b,c,d) = (replicate n a, replicate n b, replicate n c, replicate n d)

instance (Monoidal a, Monoidal b, Monoidal c, Monoidal d, Monoidal e) => Monoidal (a,b,c,d,e) where
  zero = (zero,zero,zero,zero,zero)
  replicate n (a,b,c,d,e) = (replicate n a, replicate n b, replicate n c, replicate n d, replicate n e)

