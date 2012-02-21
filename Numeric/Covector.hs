{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Numeric.Covector
  ( Covector(..)
  , ($*)
  -- * Covectors as linear functionals
  , counitM
  , unitM
  , comultM
  , multM
  , invM
  , coinvM
  , antipodeM
  , convolveM
  , memoM
  ) where

import Numeric.Additive.Class
import Numeric.Additive.Group
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Algebra.Idempotent
import Numeric.Algebra.Involutive
import Numeric.Algebra.Commutative
import Numeric.Algebra.Hopf
import Numeric.Module.Class
import Numeric.Rig.Class
import Numeric.Ring.Class
import Control.Applicative
import Control.Monad
import Data.Key
import Data.Functor.Representable.Trie
import Data.Functor.Plus hiding (zero)
import qualified Data.Functor.Plus as Plus
import Data.Functor.Bind
import qualified Prelude
import Prelude hiding ((+),(-),negate,subtract,replicate,(*))

-- | Linear functionals from elements of an (infinite) free module to a scalar

-- f $* (x + y) = (f $* x) + (f $* y)
-- f $* (a .* x) = a * (f $* x)

newtype Covector r a = Covector ((a -> r) -> r)

infixr 0 $*

($*) :: Indexable m => Covector r (Key m) -> m r -> r
Covector f $* m = f (index m)

instance Functor (Covector r) where
  fmap f m = Covector $ \k -> m $* k . f

instance Apply (Covector r) where
  mf <.> ma = Covector $ \k -> mf $* \f -> ma $* k . f

instance Applicative (Covector r) where
  pure a = Covector $ \k -> k a
  mf <*> ma = Covector $ \k -> mf $* \f -> ma $* k . f

instance Bind (Covector r) where
  m >>- f = Covector $ \k -> m $* \a -> f a $* k
  
instance Monad (Covector r) where
  return a = Covector $ \k -> k a
  m >>= f = Covector $ \k -> m $* \a -> f a $* k

instance Additive r => Alt (Covector r) where
  Covector m <!> Covector n = Covector $ m + n

instance Monoidal r => Plus (Covector r) where
  zero = Covector zero 

instance Monoidal r => Alternative (Covector r) where
  Covector m <|> Covector n = Covector $ m + n
  empty = Covector zero

instance Monoidal r => MonadPlus (Covector r) where
  Covector m `mplus` Covector n = Covector $ m + n
  mzero = Covector zero

instance Additive r => Additive (Covector r a) where
  Covector m + Covector n = Covector $ m + n
  sinnum1p n (Covector m) = Covector $ sinnum1p n m

instance Coalgebra r m => Multiplicative (Covector r m) where
  Covector f * Covector g = Covector $ \k -> f (\m -> g (comult k m))

instance (Commutative m, Coalgebra r m) => Commutative (Covector r m)

instance Coalgebra r m => Semiring (Covector r m)

instance CounitalCoalgebra r m => Unital (Covector r m) where
  one = Covector counit

instance (Rig r, CounitalCoalgebra r m) => Rig (Covector r m)

instance (Ring r, CounitalCoalgebra r m) => Ring (Covector r m)

instance Idempotent r => Idempotent (Covector r a)

instance (Idempotent r, IdempotentCoalgebra r a) => Band (Covector r a)

multM :: Coalgebra r c => c -> c -> Covector r c
multM a b = Covector $ \k -> comult k a b

unitM :: CounitalCoalgebra r c => Covector r c
unitM = Covector counit

comultM :: Algebra r a => a -> Covector r (a,a)
comultM c = Covector $ \k -> mult (curry k) c 

counitM :: UnitalAlgebra r a => a -> Covector r ()
counitM a = Covector $ \k -> unit (k ()) a

convolveM :: (Algebra r c, Coalgebra r a) => (c -> Covector r a) -> (c -> Covector r a) -> c -> Covector r a
convolveM f g c = do
   (c1,c2) <- comultM c
   a1 <- f c1
   a2 <- g c2
   multM a1 a2

invM :: InvolutiveAlgebra r h => h -> Covector r h
invM = Covector . flip inv

coinvM :: InvolutiveCoalgebra r h => h -> Covector r h
coinvM = Covector . flip coinv

-- | convolveM antipodeM return = convolveM return antipodeM = comultM >=> uncurry joinM
antipodeM :: HopfAlgebra r h => h -> Covector r h
antipodeM = Covector . flip antipode

memoM :: HasTrie a => a -> Covector s a
memoM = Covector . flip memo

-- TODO: we can also build up the augmentation ideal

instance Monoidal s => Monoidal (Covector s a) where
  zero = Covector zero
  sinnum n (Covector m) = Covector (sinnum n m)

instance Abelian s => Abelian (Covector s a)

instance Group s => Group (Covector s a) where
  Covector m - Covector n = Covector $ m - n
  negate (Covector m) = Covector $ negate m
  subtract (Covector m) (Covector n) = Covector $ subtract m n
  times n (Covector m) = Covector $ times n m

instance Coalgebra r m => LeftModule (Covector r m) (Covector r m) where
  (.*) = (*)

instance LeftModule r s => LeftModule r (Covector s m) where
  s .* m = Covector $ \k -> s .* (m $* k)

instance Coalgebra r m => RightModule (Covector r m) (Covector r m) where
  (*.) = (*)

instance RightModule r s => RightModule r (Covector s m) where
  m *. s = Covector $ \k -> (m $* k) *. s
