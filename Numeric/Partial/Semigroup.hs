module Numeric.Partial.Semigroup
  ( PartialSemigroup(..)
  ) where

import Control.Applicative
import Data.Word
import Data.Int
import Numeric.Natural.Internal

class PartialSemigroup a where
  padd :: a -> a -> Maybe a

paddNum :: Num a => a -> a -> Maybe a
paddNum a b = Just (a + b)


instance PartialSemigroup Int where
  padd = paddNum

instance PartialSemigroup Integer where
  padd = paddNum

instance PartialSemigroup Natural where
  padd = paddNum

instance PartialSemigroup Int8 where
  padd = paddNum

instance PartialSemigroup Int16 where
  padd = paddNum

instance PartialSemigroup Int32 where
  padd = paddNum

instance PartialSemigroup Int64 where
  padd = paddNum

instance PartialSemigroup Word where
  padd = paddNum

instance PartialSemigroup Word8 where
  padd = paddNum

instance PartialSemigroup Word16 where
  padd = paddNum

instance PartialSemigroup Word32 where
  padd = paddNum

instance PartialSemigroup Word64 where
  padd = paddNum

instance PartialSemigroup a => PartialSemigroup (Maybe a) where
  padd ma mb = Just $ do
   a <- ma
   b <- mb
   padd a b

instance PartialSemigroup Bool where
  padd a b = Just (a || b)

instance PartialSemigroup () where
  padd _ _ = Just ()

instance (PartialSemigroup a, PartialSemigroup b) => PartialSemigroup (a, b) where
  padd (a,b) (i,j) = (,) <$> padd a i <*> padd b j

instance (PartialSemigroup a, PartialSemigroup b, PartialSemigroup c) => PartialSemigroup (a, b, c) where
  padd (a,b,c) (i,j,k) = (,,) <$> padd a i <*> padd b j <*> padd c k

instance (PartialSemigroup a, PartialSemigroup b, PartialSemigroup c, PartialSemigroup d) => PartialSemigroup (a, b, c, d) where
  padd (a,b,c,d) (i,j,k,l) = (,,,) <$> padd a i <*> padd b j <*> padd c k <*> padd d l

instance (PartialSemigroup a, PartialSemigroup b, PartialSemigroup c, PartialSemigroup d, PartialSemigroup e) => PartialSemigroup (a, b, c, d, e) where
  padd (a,b,c,d,e) (i,j,k,l,m) = (,,,,) <$> padd a i <*> padd b j <*> padd c k <*> padd d l <*> padd e m

instance (PartialSemigroup a, PartialSemigroup b) => PartialSemigroup (Either a b) where
  padd (Left a) (Left b) = Left <$> padd a b
  padd (Right a) (Right b) = Right <$> padd a b
  padd _ _ = Nothing
