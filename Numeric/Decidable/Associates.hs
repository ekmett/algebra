module Numeric.Decidable.Associates 
  ( DecidableAssociates(..)
  , isAssociateIntegral
  , isAssociateWhole
  ) where

import Data.Function (on)
import Data.Int
import Data.Word
import Numeric.Monoid.Multiplicative
import Numeric.Natural

isAssociateIntegral :: Num n => n -> n -> Bool
isAssociateIntegral = (==) `on` abs

isAssociateWhole :: Eq n => n -> n -> Bool
isAssociateWhole = (==)

class Unital r => DecidableAssociates r where
  -- | b is an associate of a if there exists a unit u such that b = a*u
  --
  -- This relationship is symmetric because if u is a unit, u^-1 exists and is a unit, so
  -- 
  -- > b*u^-1 = a*u*u^-1 = a
  isAssociate :: r -> r -> Bool

instance DecidableAssociates Bool where isAssociate = (==)
instance DecidableAssociates Integer where isAssociate = isAssociateIntegral
instance DecidableAssociates Int where isAssociate = isAssociateIntegral
instance DecidableAssociates Int8 where isAssociate = isAssociateIntegral
instance DecidableAssociates Int16 where isAssociate = isAssociateIntegral
instance DecidableAssociates Int32 where isAssociate = isAssociateIntegral
instance DecidableAssociates Int64 where isAssociate = isAssociateIntegral

instance DecidableAssociates Natural where isAssociate = isAssociateWhole
instance DecidableAssociates Word where isAssociate = isAssociateWhole
instance DecidableAssociates Word8 where isAssociate = isAssociateWhole
instance DecidableAssociates Word16 where isAssociate = isAssociateWhole
instance DecidableAssociates Word32 where isAssociate = isAssociateWhole
instance DecidableAssociates Word64 where isAssociate = isAssociateWhole

instance DecidableAssociates () where isAssociate _ _ = True

instance (DecidableAssociates a, DecidableAssociates b) => DecidableAssociates (a, b) where
  isAssociate (a,b) (i,j) = isAssociate a i && isAssociate b j

instance (DecidableAssociates a, DecidableAssociates b, DecidableAssociates c) => DecidableAssociates (a, b, c) where
  isAssociate (a,b,c) (i,j,k) = isAssociate a i && isAssociate b j && isAssociate c k

instance (DecidableAssociates a, DecidableAssociates b, DecidableAssociates c, DecidableAssociates d) => DecidableAssociates (a, b, c, d) where
  isAssociate (a,b,c,d) (i,j,k,l) = isAssociate a i && isAssociate b j && isAssociate c k && isAssociate d l

instance (DecidableAssociates a, DecidableAssociates b, DecidableAssociates c, DecidableAssociates d, DecidableAssociates e) => DecidableAssociates (a, b, c, d, e) where
  isAssociate (a,b,c,d,e) (i,j,k,l,m) = isAssociate a i && isAssociate b j && isAssociate c k && isAssociate d l && isAssociate e m
