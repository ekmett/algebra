{-# LANGUAGE TypeOperators #-}
module Numeric.Additive.Class
  ( 
  -- * Additive Semigroups
    Additive(..)
  , sum1
  -- * Additive Abelian semigroups
  , Abelian
  -- * Additive Monoids
  , Idempotent
  , sinnum1pIdempotent
  -- * Partitionable semigroups
  , Partitionable(..)
  ) where

import Data.Int
import Data.Word
import Data.Foldable hiding (concat)
import Data.Semigroup.Foldable
import Numeric.Natural.Internal
import Prelude ((-),Bool(..),($),id,(>>=),fromIntegral,(*),otherwise,quot,maybe,error,even,Maybe(..),(==),(.),($!),Integer,(||),toInteger)
import qualified Prelude
import Data.List.NonEmpty (NonEmpty(..), fromList)

infixl 6 +

-- | 
-- > (a + b) + c = a + (b + c)
-- > sinnum 1 a = a
-- > sinnum (2 * n) a = sinnum n a + sinnum n a
-- > sinnum (2 * n + 1) a = sinnum n a + sinnum n a + a
class Additive r where
  (+) :: r -> r -> r

  -- | sinnum1p n r = sinnum (1 + n) r
  sinnum1p :: Whole n => n -> r -> r
  sinnum1p y0 x0 = f x0 (1 Prelude.+ y0)
    where
      f x y
        | even y = f (x + x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x + x) (unsafePred y  `quot` 2) x
      g x y z
        | even y = g (x + x) (y `quot` 2) z
        | y == 1 = x + z
        | otherwise = g (x + x) (unsafePred y `quot` 2) (x + z)

  sumWith1 :: Foldable1 f => (a -> r) -> f a -> r
  sumWith1 f = maybe (error "Numeric.Additive.Semigroup.sumWith1: empty structure") id . foldl' mf Nothing
     where mf Nothing y = Just $! f y 
           mf (Just x) y = Just $! x + f y

sum1 :: (Foldable1 f, Additive r) => f r -> r
sum1 = sumWith1 id

instance Additive r => Additive (b -> r) where
  f + g = \e -> f e + g e 
  sinnum1p n f e = sinnum1p n (f e)
  sumWith1 f xs e = sumWith1 (`f` e) xs

instance Additive Bool where
  (+) = (||)
  sinnum1p _ a = a

instance Additive Natural where
  (+) = (Prelude.+)
  sinnum1p n r = (1 Prelude.+ toNatural n) * r

instance Additive Integer where 
  (+) = (Prelude.+)
  sinnum1p n r = (1 Prelude.+ toInteger n) * r

instance Additive Int where
  (+) = (Prelude.+)
  sinnum1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Int8 where
  (+) = (Prelude.+)
  sinnum1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Int16 where
  (+) = (Prelude.+)
  sinnum1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Int32 where
  (+) = (Prelude.+)
  sinnum1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Int64 where
  (+) = (Prelude.+)
  sinnum1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Word where
  (+) = (Prelude.+)
  sinnum1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Word8 where
  (+) = (Prelude.+)
  sinnum1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Word16 where
  (+) = (Prelude.+)
  sinnum1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Word32 where
  (+) = (Prelude.+)
  sinnum1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive Word64 where
  (+) = (Prelude.+)
  sinnum1p n r = fromIntegral (1 Prelude.+ n) * r

instance Additive () where
  _ + _ = ()
  sinnum1p _ _ = () 
  sumWith1 _ _ = ()

instance (Additive a, Additive b) => Additive (a,b) where
  (a,b) + (i,j) = (a + i, b + j)
  sinnum1p n (a,b) = (sinnum1p n a, sinnum1p n b)

instance (Additive a, Additive b, Additive c) => Additive (a,b,c) where
  (a,b,c) + (i,j,k) = (a + i, b + j, c + k)
  sinnum1p n (a,b,c) = (sinnum1p n a, sinnum1p n b, sinnum1p n c)

instance (Additive a, Additive b, Additive c, Additive d) => Additive (a,b,c,d) where
  (a,b,c,d) + (i,j,k,l) = (a + i, b + j, c + k, d + l)
  sinnum1p n (a,b,c,d) = (sinnum1p n a, sinnum1p n b, sinnum1p n c, sinnum1p n d)

instance (Additive a, Additive b, Additive c, Additive d, Additive e) => Additive (a,b,c,d,e) where
  (a,b,c,d,e) + (i,j,k,l,m) = (a + i, b + j, c + k, d + l, e + m)
  sinnum1p n (a,b,c,d,e) = (sinnum1p n a, sinnum1p n b, sinnum1p n c, sinnum1p n d, sinnum1p n e)


concat :: NonEmpty (NonEmpty a) -> NonEmpty a
concat m = m >>= id

class Additive m => Partitionable m where
  -- | partitionWith f c returns a list containing f a b for each a b such that a + b = c, 
  partitionWith :: (m -> m -> r) -> m -> NonEmpty r

instance Partitionable Bool where
  partitionWith f False = f False False :| []
  partitionWith f True  = f False True :| [f True False, f True True]

instance Partitionable Natural where
  partitionWith f n = fromList [ f k (n - k) | k <- [0..n] ]

instance Partitionable () where
  partitionWith f () = f () () :| []

instance (Partitionable a, Partitionable b) => Partitionable (a,b) where
  partitionWith f (a,b) = concat $ partitionWith (\ax ay -> 
                                   partitionWith (\bx by -> f (ax,bx) (ay,by)) b) a

instance (Partitionable a, Partitionable b, Partitionable c) => Partitionable (a,b,c) where
  partitionWith f (a,b,c) = concat $ partitionWith (\ax ay -> 
                            concat $ partitionWith (\bx by -> 
                                     partitionWith (\cx cy -> f (ax,bx,cx) (ay,by,cy)) c) b) a

instance (Partitionable a, Partitionable b, Partitionable c,Partitionable d ) => Partitionable (a,b,c,d) where
  partitionWith f (a,b,c,d) = concat $ partitionWith (\ax ay -> 
                              concat $ partitionWith (\bx by -> 
                              concat $ partitionWith (\cx cy -> 
                                       partitionWith (\dx dy -> f (ax,bx,cx,dx) (ay,by,cy,dy)) d) c) b) a

instance (Partitionable a, Partitionable b, Partitionable c,Partitionable d, Partitionable e) => Partitionable (a,b,c,d,e) where
  partitionWith f (a,b,c,d,e) = concat $ partitionWith (\ax ay -> 
                                concat $ partitionWith (\bx by -> 
                                concat $ partitionWith (\cx cy -> 
                                concat $ partitionWith (\dx dy -> 
                                         partitionWith (\ex ey -> f (ax,bx,cx,dx,ex) (ay,by,cy,dy,ey)) e) d) c) b) a


-- | an additive abelian semigroup
--
-- a + b = b + a
class Additive r => Abelian r

instance Abelian r => Abelian (e -> r)
instance Abelian ()
instance Abelian Bool
instance Abelian Integer
instance Abelian Natural
instance Abelian Int
instance Abelian Int8
instance Abelian Int16
instance Abelian Int32
instance Abelian Int64
instance Abelian Word
instance Abelian Word8
instance Abelian Word16
instance Abelian Word32
instance Abelian Word64
instance (Abelian a, Abelian b) => Abelian (a,b) 
instance (Abelian a, Abelian b, Abelian c) => Abelian (a,b,c) 
instance (Abelian a, Abelian b, Abelian c, Abelian d) => Abelian (a,b,c,d) 
instance (Abelian a, Abelian b, Abelian c, Abelian d, Abelian e) => Abelian (a,b,c,d,e) 

-- | An additive semigroup with idempotent addition.
--
-- > a + a = a
--
class Additive r => Idempotent r

sinnum1pIdempotent :: Natural -> r -> r
sinnum1pIdempotent _ r = r

instance Idempotent ()
instance Idempotent Bool
instance Idempotent r => Idempotent (e -> r)
instance (Idempotent a, Idempotent b) => Idempotent (a,b)
instance (Idempotent a, Idempotent b, Idempotent c) => Idempotent (a,b,c)
instance (Idempotent a, Idempotent b, Idempotent c, Idempotent d) => Idempotent (a,b,c,d)
instance (Idempotent a, Idempotent b, Idempotent c, Idempotent d, Idempotent e) => Idempotent (a,b,c,d,e)
