{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Algebra.Unital
  ( 
  -- * Unital Multiplication (Multiplicative monoid)
    Unital(..)
  , product
  -- * Unital Associative Algebra 
  , UnitalAlgebra(..)
  -- * Unital Coassociative Coalgebra
  , CounitalCoalgebra(..)
  -- * Bialgebra
  , Bialgebra
  ) where

import Numeric.Algebra.Class
import Numeric.Natural.Internal
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable hiding (product)
import Data.Int
import Data.Word
import Prelude hiding ((*), foldr, product)

infixr 8 `pow`

class Multiplicative r => Unital r where
  one :: r
  pow :: Whole n => r -> n -> r
  pow _ 0 = one
  pow x0 y0 = f x0 y0 where
    f x y 
      | even y = f (x * x) (y `quot` 2)
      | y == 1 = x
      | otherwise = g (x * x) ((y - 1) `quot` 2) x
    g x y z 
      | even y = g (x * x) (y `quot` 2) z
      | y == 1 = x * z
      | otherwise = g (x * x) ((y - 1) `quot` 2) (x * z)
  productWith :: Foldable f => (a -> r) -> f a -> r
  productWith f = foldl' (\b a -> b * f a) one

product :: (Foldable f, Unital r) => f r -> r
product = productWith id

instance Unital Bool where one = True
instance Unital Integer where one = 1
instance Unital Int where one = 1
instance Unital Int8 where one = 1
instance Unital Int16 where one = 1
instance Unital Int32 where one = 1
instance Unital Int64 where one = 1
instance Unital Natural where one = 1
instance Unital Word where one = 1
instance Unital Word8 where one = 1
instance Unital Word16 where one = 1
instance Unital Word32 where one = 1
instance Unital Word64 where one = 1
instance Unital () where one = ()
instance (Unital a, Unital b) => Unital (a,b) where
  one = (one,one)

instance (Unital a, Unital b, Unital c) => Unital (a,b,c) where
  one = (one,one,one)

instance (Unital a, Unital b, Unital c, Unital d) => Unital (a,b,c,d) where
  one = (one,one,one,one)

instance (Unital a, Unital b, Unital c, Unital d, Unital e) => Unital (a,b,c,d,e) where
  one = (one,one,one,one,one)

-- | An associative unital algebra over a semiring, built using a free module
class Algebra r a => UnitalAlgebra r a where
  unit :: r -> a -> r

instance (Unital r, UnitalAlgebra r a) => Unital (a -> r) where
  one = unit one

instance Semiring r => UnitalAlgebra r () where
  unit r () = r

-- incoherent
-- instance UnitalAlgebra () a where unit _ _ = ()
-- instance (UnitalAlgebra r a, UnitalAlgebra r b) => UnitalAlgebra (a -> r) b where unit f b a = unit (f a) b

instance (UnitalAlgebra r a, UnitalAlgebra r b) => UnitalAlgebra r (a,b) where
  unit r (a,b) = unit r a * unit r b

instance (UnitalAlgebra r a, UnitalAlgebra r b, UnitalAlgebra r c) => UnitalAlgebra r (a,b,c) where
  unit r (a,b,c) = unit r a * unit r b * unit r c

instance (UnitalAlgebra r a, UnitalAlgebra r b, UnitalAlgebra r c, UnitalAlgebra r d) => UnitalAlgebra r (a,b,c,d) where
  unit r (a,b,c,d) = unit r a * unit r b * unit r c * unit r d

instance (UnitalAlgebra r a, UnitalAlgebra r b, UnitalAlgebra r c, UnitalAlgebra r d, UnitalAlgebra r e) => UnitalAlgebra r (a,b,c,d,e) where
  unit r (a,b,c,d,e) = unit r a * unit r b * unit r c * unit r d * unit r e

instance (Monoidal r, Semiring r) => UnitalAlgebra r [a] where
  unit r [] = r
  unit _ _ = zero

instance (Monoidal r, Semiring r) => UnitalAlgebra r (Seq a) where
  unit r a | Seq.null a = r
           | otherwise = zero

-- A coassociative counital coalgebra over a semiring, where the module is free
class Coalgebra r c => CounitalCoalgebra r c where
  counit :: (c -> r) -> r

instance (Unital r, UnitalAlgebra r m) => CounitalCoalgebra r (m -> r) where
  counit k = k one

-- incoherent
-- instance (UnitalAlgebra r a, CounitalCoalgebra r c) => CounitalCoalgebra (a -> r) c where counit k a = counit (`k` a)
-- instance CounitalCoalgebra () a where counit _ = ()

instance Semiring r => CounitalCoalgebra r () where
  counit f = f ()

instance (CounitalCoalgebra r a, CounitalCoalgebra r b) => CounitalCoalgebra r (a, b) where
  counit k = counit $ \a -> counit $ \b -> k (a,b)

instance (CounitalCoalgebra r a, CounitalCoalgebra r b, CounitalCoalgebra r c) => CounitalCoalgebra r (a, b, c) where
  counit k = counit $ \a -> counit $ \b -> counit $ \c -> k (a,b,c)

instance (CounitalCoalgebra r a, CounitalCoalgebra r b, CounitalCoalgebra r c, CounitalCoalgebra r d) => CounitalCoalgebra r (a, b, c, d) where
  counit k = counit $ \a -> counit $ \b -> counit $ \c -> counit $ \d -> k (a,b,c,d)

instance (CounitalCoalgebra r a, CounitalCoalgebra r b, CounitalCoalgebra r c, CounitalCoalgebra r d, CounitalCoalgebra r e) => CounitalCoalgebra r (a, b, c, d, e) where
  counit k = counit $ \a -> counit $ \b -> counit $ \c -> counit $ \d -> counit $ \e -> k (a,b,c,d,e)

instance Semiring r => CounitalCoalgebra r [a] where
  counit k = k []

instance Semiring r => CounitalCoalgebra r (Seq a) where
  counit k = k (Seq.empty)

-- | A bialgebra is both a unital algebra and counital coalgebra 
-- where the `mult` and `unit` are compatible in some sense with 
-- the `comult` and `counit`. That is to say that 
-- 'mult' and 'unit' are a coalgebra homomorphisms or (equivalently) that 
-- 'comult' and 'counit' are an algebra homomorphisms.

class (UnitalAlgebra r a, CounitalCoalgebra r a) => Bialgebra r a

-- TODO
-- instance (Unital r, Bialgebra r m) => Bialgebra r (m -> r)
-- instance Bialgebra () c
-- instance (UnitalAlgebra r b, Bialgebra r c) => Bialgebra (b -> r) c

instance Semiring r => Bialgebra r ()
instance (Bialgebra r a, Bialgebra r b) => Bialgebra r (a, b)
instance (Bialgebra r a, Bialgebra r b, Bialgebra r c) => Bialgebra r (a, b, c)
instance (Bialgebra r a, Bialgebra r b, Bialgebra r c, Bialgebra r d) => Bialgebra r (a, b, c, d)
instance (Bialgebra r a, Bialgebra r b, Bialgebra r c, Bialgebra r d, Bialgebra r e) => Bialgebra r (a, b, c, d, e)

instance (Monoidal r, Semiring r) => Bialgebra r [a]
instance (Monoidal r, Semiring r) => Bialgebra r (Seq a)
