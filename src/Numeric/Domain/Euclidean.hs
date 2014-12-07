module Numeric.Domain.Euclidean (Euclidean(..), euclid, prs, chineseRemainder) where
import Numeric.Additive.Group
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Decidable.Zero
import Numeric.Domain.Internal
import Prelude (otherwise)
import qualified Prelude                 as P

prs :: Euclidean r => r -> r -> [(r, r, r)]
prs f g = step [(g, zero, one), (f, one, zero)]
  where
    step acc@((r',s',t'):(r,s,t):_)
      | isZero r' = P.tail acc
      | otherwise =
        let q         = r `quot` r'
            s''       = (s - q * s')
            t''       = (t - q * t')
        in step ((r - q * r', s'', t'') : acc)
    step _ = P.error "cannot happen!"

chineseRemainder :: Euclidean r
                 => [(r, r)] -- ^ List of @(m_i, v_i)@
                 -> r        -- ^ @f@ with @f@ = @v_i@ (mod @v_i@)
chineseRemainder mvs =
  let (ms, _) = P.unzip mvs
      m = product ms
  in sum [((vi*s) `rem` mi)*n | (mi, vi) <- mvs
                               , let n = m `quot` mi
                               , let (_, s, _) : _ = euclid n mi
                               ]
