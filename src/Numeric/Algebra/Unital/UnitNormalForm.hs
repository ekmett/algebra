module Numeric.Algebra.Unital.UnitNormalForm 
    (UnitNormalForm(..), normalize, leadingUnit) where

import Numeric.Algebra.Unital
import Prelude(Integer,signum,abs,fst,snd,(.))

class (Unital r) => UnitNormalForm r where
    -- prop> let (u,n) = splitUnit r
    --           (u',n') = splitUnit r' in
    --           isUnit u && isUnit u' &&
    --           u*n = r && u'*n' = r' &&
    --           (isAssociate r r' ==> n = n') &&
    --           splitUnit (r * r') = (u * u', n * n')
    splitUnit :: r -> (r,r)

instance UnitNormalForm Integer where
  splitUnit 0 = (1, 0)
  splitUnit n = (signum n, abs n)
  {-# INLINE splitUnit #-}


normalize :: UnitNormalForm r => r -> r
normalize = snd . splitUnit

leadingUnit :: UnitNormalForm r => r -> r
leadingUnit = fst . splitUnit

