module Numeric.Algebra where

import Numeric.Semiring.Class
import Numeric.Addition
import Numeric.Map.Linear
import Prelude hiding ((+),(-))

-- associative algebras over a free semiring module

class Semiring r => Algebra r a where
  join :: (a -> a -> r) -> a -> r

instance Algebra r a => Multiplicative (a -> r) where
  f * g = join (\a b -> f a * g b) 

-- associative unital algebras over a free semiring module
class Algebra r a => UnitalAlgebra r a where
  unit :: r -> a -> r

instance (MultiplicativeMonoid r, UnitalAlgebra r a) => MultiplicativeMonoid (a -> r) where
  one = unit one

class Semiring r => Coalgebra r c where
  cojoin :: (c -> r) -> c -> c -> r

class Coalgebra r => UnitalCoalgebra r c where
  counit :: (c -> r) -> r  

--instance Coalgebra r m -> Algebra r (m -> r) where
--  unit :: r -> (m -> r) -> r
instance

instance Algebra r m => Coalgebra r (m -> r) where
  cojoin k f g = k (f * g)

instance Coalgebra r m => Algebra r (m -> r) where
  join k m = 
  
  join :: ((m -> r) -> (m -> r) -> r) -> m -> r
  given :: (m -> r) -> m -> m -> r
  
instance Coalgebra r m => Multiplicative (Linear r m) where
  Linear f * Linear g = Linear (\k -> f (g . cojoin k)))

instance Coalgebra r m => MultiplicativeMonoid (Linear r m) where
  one = Linear counit

-- take the product of two linear maps
-- prod :: Linear r a b -> Linear r c d -> Linear r (a,c) (b,d)
-- prod f g = Linear $ \k (a,c) -> appLinear f (\b -> appLinear g (\d -> k (b,d)) c) a
-- convolve :: (Algebra r a, Coalgebra r c) => Linear r a c -> Linear r a c -> Linear r a c
-- convolve (Linear f) (Linear g) = join . prod f g . cojoin 

prod :: ((b -> r) -> a -> r) -> ((d -> r) -> c -> r) -> (b -> d -> r) -> a -> c -> r
prod f g k a c = f (\b -> g (k b) c) a

convolve :: (Algebra r a, Coalgebra r c) => ((c -> r) -> a -> r) -> ((c -> r) -> a -> r) -> (c -> r) -> a -> r
convolve (Linear f) (Linear g) = join . prod f g . cojoin 

-- if antipode . antipode = id then we are involutive
class (Algebra r h, Coalgebra r h) => Hopf r h where
  -- convolve id antipode = convolve antipode id = unit . counit
  antipode :: (h -> r) -> (h -> r)

newtype HopfGroup g = HopfGroup g 

-- the Hopf algebra over the field with one element 
instance AdditiveGroup g => Algebra () (HopfGroup g) where
  unit _ _ = ()
  join f a = f a a 

instance AdditiveGroup g => Coalgebra () (HopfGroup g) where
  counit _ = ()
  cojoin f c c = f (c + c)

instance Hopf () (HopfGroup g) where
  antipode f h = f (negate h)

-- polynomial power basis
newtype P = P Integer

instance Semiring r => Coalgebra r P where
  cojoin f (P n) (P m) = f (P (n + m))
  counit f = f (P 0)

newtype PN = PN Natural 

instance (Semiring r, AdditiveMonoid r) => Algebra r PN where
  unit r (PN 0) = r
  unit r _ = zero
  join f (PN n) = sum [ f (PN k) (PN (n - k)) | k <- [0 .. n] ]

instance Semiring r => Coalgebra r PN where
  cojoin f (PN n) (PN m) = f (P (n + m))
  counit f = f (PN 0)

-- chebyshev polynomials of the first kind
newtype U = U Int
instance (Semiring r, MultiplicativeGroup r) => Coalgebra r U where
  cojoin f (U n) (U m) = (f (U (n + m)) + f (U (abs (n - m)))) /. 2

instance (Semiring r, MulitplicativeGroup r) => UnitalCoalgebra r U where
  counit f = f (U 0)

-- the semigroup semiring, monoid ring, or group ring over a semigroup m
newtype M m = M m 

instance Multiplicative m => Multiplicative (M m) where
  M m * M n = M (m * n)

instance Factorable m => Factorable (M m) where
  factorWith f (M m) = factorWith (on f M) m

instance (Multiplicative m, Semiring r) => Coalgebra r (M m) where
  cojoin f (M a) (M b) = f (M (a * b))

instance (MultiplicativeMonoid m, Semiring r) => Coalgebra r (M m) where
  counit f = f (M one)

instance (Factorable m, Semiring r) => Algebra r (M m) where
  join f = sum . factorWith f

instance (Factorable m, MultiplicativeMonoid m, Rig r) => UnitalAlgebra r (M m) where
  unit r a | a == one = one
           | otherwise = zero

-- 

class Semiring r => Algebra r m where
  join :: Linear r m -> Linear r m -> Linear r m
  unit :: Linear r m


Linear f * Linear g = Linear (\k -> f (\(U i) -> g (\(U j) -> (k (U (i + j)) + k U (abs (i - j))) /. 2)) 
one :: Linear r U 
one = Linear (\
