module Numeric.Decidable.Associates 
  ( DecidableAssociates
  , isAssociateIntegral
  ) where

import Data.Function (on)
import Data.Int
import Data.Word
import Numeric.Multiplicative.Monoid

isAssociateIntegral :: Integral n => n -> n -> Bool
isAssociateIntegral = (==) `on` abs

class MultiplicativeMonoid r => DecidableAssociates r where
  -- | b is an associate of a if there exists a unit u such that b = au
  isAssociate :: r -> r -> Bool

instance DecidableAssociates Int where isAssociate = isAssociateIntegral
instance DecidableAssociates Int8 where isAssociate = isAssociateIntegral
instance DecidableAssociates Int16 where isAssociate = isAssociateIntegral
instance DecidableAssociates Int32 where isAssociate = isAssociateIntegral
instance DecidableAssociates Int64 where isAssociate = isAssociateIntegral
instance DecidableAssociates Word where isAssociate = isAssociateIntegral
instance DecidableAssociates Word8 where isAssociate = isAssociateIntegral
instance DecidableAssociates Word16 where isAssociate = isAssociateIntegral
instance DecidableAssociates Word32 where isAssociate = isAssociateIntegral
instance DecidableAssociates Word64 where isAssociate = isAssociateIntegral
instance DecidableAssociates Integer where isAssociate = isAssociateIntegral
