module Numeric.Addition.Partitionable
  ( -- * Partitionable Additive Semigroups
    Partitionable(..)
  ) where

import Prelude ((-),Bool(..),($),id,(>>=))
import Numeric.Semigroup.Additive
import Numeric.Natural
import Data.List.NonEmpty (NonEmpty(..), fromList)

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
