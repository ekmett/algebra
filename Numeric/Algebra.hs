module Numeric.Algebra where

import Numeric.Semiring.Class
import Numeric.Additive

-- algebras over a free module

class Semiring r => Algebra r a where
  unit :: r -> a -> r
  join :: (a -> a -> r) -> a -> r
  
class Semiring r => Coalgebra r c where
  cojoin :: (c -> r) -> c -> c -> r
  counit :: (c -> r) -> r  

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
newtype P x = P Integer

instance Semiring r => Coalgebra r (P x) where
  cojoin f (P n) (P m) = f (P (n + m))
  counit f = f (P 0)

newtype PN x = P Natural 

instance (Semiring r, AdditiveMonoid r) => Algebra r (PN x) where
  unit r (PN 0) = r
  unit r _     = zero
  join f (PN n) = sum [ f (PN k) (PN (n - k)) | k <- [0 .. n] ]

instance Semiring r => Coalgebra r (PN x) where
  cojoin f (PN n) (PN m) = f (P (n + m))
  counit f = f (PN 0)

-- newtype U x = U Int
-- instance Semiring r => Coalgebra r (U x) where
