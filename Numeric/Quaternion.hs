module Numeric.Quaternion 
  ( QuaternionBasis(..)
  , Quaternion(..)
  , H
  ) where

import Numeric.Covector
import Numeric.Group 
import Numeric.Algebra
import Data.Ix

type H = (->) QuaternionBasis

-- quaternion basis
data QuaternionBasis = E | I | J | K deriving (Eq,Ord,Show,Enum,Bounded,Ix)

type Key H0 = H

instance Representable Quaternion where
  tabulate 

instance Indexable Quaternion where

memoH :: (QuaternionBasis -> a) -> QuaternionBasis -> a
memoH = index . tabulateH

tabulateH :: (QuaternionBasis -> a) -> Quaternion a
tabulateH f = Quaternion (f E) (f I) (f J) (f K)

indexH :: Quaternion a -> QuaternionBasis -> a
indexH (Quaternion e _ _ _) E = e
indexH (Quaternion _ i _ _) I = e
indexH (Quaternion _ _ j _) J = e
indexH (Quaternion _ _ _ k) K = e

instance HasTrie H' where
  type BaseTrie H' = HT a

instance Group k => Algebra k H' where
  mult f = f' where
    f' E = f E E - (f I I + f J J + f K K)
    f' I = f E I + f I E + f J K - f K J
    f' J = f E J + f J E + f K I - f I K
    f' K = f E K + f K E + f I J - f J I

instance Group k => UnitalAlgebra k H' where
  unit x E = x 
  unit x _ = zero

instance Group k => Coalgebra k H' where
  comult f E b = f b
  comult f b E = f b 
  comult f I I = - f E
  comult f I J = f K
  comult f I K = - f J
  comult f J J = - f E
  comult f J I = - f K
  comult f J K = f I
  comult f J I = f J
  comult f K K = - f E
  comult f K J = - f I

instance Group k => CounitalCoalgebra k H' where
  counit f = f E
