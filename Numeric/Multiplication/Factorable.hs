module Numeric.Multiplication.Factorable
  ( -- * Partitionable Additive Semigroups
    Factorable(..)
  ) where

import Data.List.NonEmpty
import Numeric.Multiplicative
import Prelude hiding (concat)

-- | `factorWith f c` returns a non-empty list containing `f a b` for all `a, b` such that `a * b = c`.
--
-- Results of factorWith f 0 are undefined and may result in either an error or an infinite list.

class Multiplicative m => Factorable m where
  factorWith :: (m -> m -> r) -> m -> NonEmpty r

instance Factorable Bool where
  factorWith f False = f False False :| [f False True, f True False]
  factorWith f True  = f True True :| []

instance Factorable () where
  factorWith f () = f () () :| []

concat :: NonEmpty (NonEmpty a) -> NonEmpty a
concat m = m >>= id

instance (Factorable a, Factorable b) => Factorable (a,b) where
  factorWith f (a,b) = concat $ factorWith (\ax ay ->
                                factorWith (\bx by -> f (ax,bx) (ay,by)) b) a

instance (Factorable a, Factorable b, Factorable c) => Factorable (a,b,c) where
  factorWith f (a,b,c) = concat $ factorWith (\ax ay ->
                            concat $ factorWith (\bx by ->
                                     factorWith (\cx cy -> f (ax,bx,cx) (ay,by,cy)) c) b) a

instance (Factorable a, Factorable b, Factorable c,Factorable d ) => Factorable (a,b,c,d) where
  factorWith f (a,b,c,d) = concat $ factorWith (\ax ay ->
                           concat $ factorWith (\bx by ->
                           concat $ factorWith (\cx cy ->
                                    factorWith (\dx dy -> f (ax,bx,cx,dx) (ay,by,cy,dy)) d) c) b) a

instance (Factorable a, Factorable b, Factorable c,Factorable d, Factorable e) => Factorable (a,b,c,d,e) where
  factorWith f (a,b,c,d,e) = concat $ factorWith (\ax ay ->
                             concat $ factorWith (\bx by ->
                             concat $ factorWith (\cx cy ->
                             concat $ factorWith (\dx dy ->
                                      factorWith (\ex ey -> f (ax,bx,cx,dx,ex) (ay,by,cy,dy,ey)) e) d) c) b) a


