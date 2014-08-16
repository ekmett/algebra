{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module Numeric.Map
  ( Map(..)
  , ($@)
  , multMap
  , unitMap
  , comultMap
  , counitMap
  , invMap
  , coinvMap
  , antipodeMap
  , convolveMap
  ) where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Reader.Class
import Data.Functor.Rep
import Data.Functor.Bind
import Data.Functor.Plus hiding (zero)
import qualified Data.Functor.Plus as Plus
import Data.Semigroupoid
import Numeric.Algebra
import Prelude hiding ((*), (+), negate, subtract,(-), recip, (/), foldr, sum, product, replicate, concat, (.), id, fst, snd)

-- | linear maps from elements of a free module to another free module over r
--
-- > f $# x + y = (f $# x) + (f $# y)
-- > f $# (r .* x) = r .* (f $# x)
--
--
-- @Map r b a@ represents a linear mapping from a free module with basis @a@ over @r@ to a free module with basis @b@ over @r@.
--
-- Note well the reversed direction of the arrow, due to the contravariance of change of basis!
--
-- This way enables we can employ arbitrary pure functions as linear maps by lifting them using `arr`, or build them
-- by using the monad instance for Map r b.  As a consequence Map is an instance of, well, almost everything.

infixr 0 $#
newtype Map r b a = Map ((a -> r) -> b -> r)

($#) :: (Representable v, Representable w) => Map r (Rep w) (Rep v) -> v r -> w r
($#) (Map m) = tabulate . m . index

infixr 0 $@
-- | extract a linear functional from a linear map
($@) :: Map r b a -> b -> Covector r a
m $@ b = Covector $ \k -> (m $# k) b

-- NB: due to contravariance (>>>) to get the usual notion of composition!
instance Category (Map r) where
  id = Map id
  Map f . Map g = Map (g . f)

instance Semigroupoid (Map r) where
  Map f `o` Map g = Map (g . f)

instance Functor (Map r b) where
  fmap f m = Map $ \k -> m $# k . f

instance Apply (Map r b) where
  mf <.> ma = Map $ \k b -> (mf $# \f -> (ma $# k . f) b) b

instance Applicative (Map r b) where
  pure a = Map $ \k _ -> k a
  mf <*> ma = Map $ \k b -> (mf $# \f -> (ma $# k . f) b) b

instance Bind (Map r b) where
  Map m >>- f = Map $ \k b -> m (\a -> (f a $# k) b) b

instance Monad (Map r b) where
  return a = Map $ \k _ -> k a
  m >>= f = Map $ \k b -> (m $# \a -> (f a $# k) b) b

instance Arrow (Map r) where
  arr f = Map (. f)
  first m = Map $ \k (a,c) -> (m $# \b -> k (b,c)) a
  second m = Map $ \k (c,a) -> (m $# \b -> k (c,b)) a
  m *** n = Map $ \k (a,c) -> (m $# \b -> (n $# \d -> k (b,d)) c) a
  m &&& n = Map $ \k a -> (m $# \b -> (n $# \c -> k (b,c)) a) a

instance ArrowApply (Map r) where
  app = Map $ \k (f,a) -> (f $# k) a

instance MonadReader b (Map r b) where
  ask = id
  local f m = Map $ \k -> (m $# k) . f

-- While the following typechecks, it isn't correct,
-- callCC is non-linear, the internal Map ignores the functional it is given!
--
--instance MonadCont (Map r b) where
--  callCC f = Map $ \k -> (f $# \a -> Map $ \_ _ -> k a) k

-- label :: ((a -> r) -> Map r b a) -> Map r b a
-- label f = Map $ \k -> f k $# k

-- break :: (a -> r) -> a -> Map r b a

instance Monoidal r => ArrowZero (Map r) where
  zeroArrow = Map zero

instance Monoidal r => ArrowPlus (Map r) where
  Map m <+> Map n = Map $ m + n

instance ArrowChoice (Map r) where
  left m = Map $ \k -> either (m $# k . Left) (k . Right)
  right m = Map $ \k -> either (k . Left) (m $# k . Right)
  m +++ n =  Map $ \k -> either (m $# k . Left) (n $# k . Right)
  m ||| n = Map $ \k -> either (m $# k) (n $# k)

-- TODO: ArrowLoop?

-- TODO: more categories instances for (Map r) & Either to get to precocartesian!

instance Additive r => Additive (Map r b a) where
  Map m + Map n = Map $ m + n
  sinnum1p n (Map m) = Map $ sinnum1p n m

instance Coalgebra r m => Multiplicative (Map r b m) where
  f * g = Map $ \k b -> (f $# \a -> (g $# comult k a) b) b
instance CounitalCoalgebra r m => Unital (Map r b m) where
  one = Map $ \k _ -> counit k

instance Coalgebra r m => Semiring (Map r b m)

instance Coalgebra r m => LeftModule (Map r b m) (Map r b m) where
  (.*) = (*)

instance LeftModule r s => LeftModule r (Map s b m) where
  s .* Map m = Map $ \k b -> s .* m k b

instance Coalgebra r m => RightModule (Map r b m) (Map r b m) where (*.) = (*)
instance RightModule r s => RightModule r (Map s b m) where
  Map m *. s = Map $ \k b -> m k b *. s

instance Additive r => Alt (Map r b) where
  Map m <!> Map n = Map $ m + n

instance Monoidal r => Plus (Map r b) where
  zero = Map zero

instance Monoidal r => Alternative (Map r b) where
  Map m <|> Map n = Map $ m + n
  empty = Map zero

instance Monoidal r => MonadPlus (Map r b) where
  Map m `mplus` Map n = Map $ m + n
  mzero = Map zero

instance Monoidal s => Monoidal (Map s b a) where
  zero = Map zero
  sinnum n (Map m) = Map $ sinnum n m

instance Abelian s => Abelian (Map s b a)

instance Group s => Group (Map s b a) where
  Map m - Map n = Map $ m - n
  negate (Map m) = Map $ negate m
  subtract (Map m) (Map n) = Map $ subtract m n
  times n (Map m) = Map $ times n m

instance (Commutative m, Coalgebra r m) => Commutative (Map r b m)

instance (Rig r, CounitalCoalgebra r m) => Rig (Map r b m)

instance (Ring r, CounitalCoalgebra r m) => Ring (Map r a m)

-- | (inefficiently) combine a linear combination of basis vectors to make a map.
-- arrMap :: (Monoidal r, Semiring r) => (b -> [(r, a)]) -> Map r b a
-- arrMap f = Map $ \k b -> sum [ r * k a | (r, a) <- f b ]

comultMap :: Algebra r a => Map r a (a,a)
comultMap = Map $ mult . curry

multMap :: Coalgebra r c => Map r (c,c) c
multMap = Map $ uncurry . comult

counitMap :: UnitalAlgebra r a => Map r a ()
counitMap = Map $ \k -> unit $ k ()

unitMap :: CounitalCoalgebra r c => Map r () c
unitMap = Map $ \k () -> counit k

-- | convolution given an associative algebra and coassociative coalgebra
convolveMap :: (Algebra r a, Coalgebra r c) => Map r a c -> Map r a c -> Map r a c
convolveMap f g = multMap . (f *** g) . comultMap

-- convolveMap antipodeMap id = convolveMap id antipodeMap = unit . counit
antipodeMap :: HopfAlgebra r h => Map r h h
antipodeMap = Map antipode

coinvMap :: InvolutiveAlgebra r a => Map r a a
coinvMap = Map inv

invMap :: InvolutiveCoalgebra r c => Map r c c
invMap = Map coinv

{-
-- ring homomorphism from r -> r^a
embedMap :: (Unital m, CounitalCoalgebra r m) => (b -> r) -> Map r b m
embedMap f = Map $ \k b -> f b * k one

-- if the characteristic of s does not divide the order of a, then s[a] is semisimple
-- and if a has a length function, we can build a filtered algebra

-- | The augmentation ring homomorphism from r^a -> r
augmentMap :: Unital s => Map s b m -> b -> s
augmentMap m = m $# const one
-}

