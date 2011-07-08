module Numeric.Ring.Endomorphism 
  ( End(..)
  , toEnd
  , fromEnd
  ) where

import Numeric.Additive
import Numeric.Multiplicative
import Numeric.Rng.Class
import Numeric.Ring.Class

-- | The endomorphism ring of an abelian group or the endomorphism semiring of an abelian monoid
-- 
-- http://en.wikipedia.org/wiki/Endomorphism_ring
newtype End a = End { appEnd :: a -> a }
instance Monoid (End r) where
  mappend (End a) (End b) = End (a . b)
  mempty = End id
instance Additive r => Additive (End r) where
  End f + End g = End (\x -> f x + g x)
instance Abelian r => Abelian (End r)
instance AdditiveMonoid r => AdditiveMonoid (End r) where
  zero = End (const zero)
instance AdditiveGroup r => AdditiveGroup (End r) where
  End f - End g = End (\x -> f x - g x)
  negate (End f) = End (negate . f)
  subtract (End f) (End g) = End (\x -> subtract (f x) (g x))
instance Multiplicative (End r) where
  End f * End g = End (f . g)
instance MultiplicativeMonoid (End r) where
  one = End id
instance (Abelian r, Commutative r) => Commutative (End r) 
instance (Abelian r, AdditiveMonoid r) => Semiring (End r)
instance (Abelian r, AdditiveGroup r) => Rng (End r)
instance (Abelian r, AdditiveGroup r) => Ring (End r)

-- instance SimpleAdditiveAbelianGroup r => DivisionRing (End r) where

-- ring isomorphism from r to the endomorphism ring of r.
toEnd :: Multiplicative r => r -> End r
toEnd r = End (*r)

-- ring isomorphism from the endormorphism ring of r to r.
fromEnd :: MultiplicativeMonoid r => End r -> r
fromEnd (End f) = f one
