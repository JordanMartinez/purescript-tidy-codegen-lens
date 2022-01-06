module Data.Traversable.Accum.Lens where

import Data.Lens.Iso (Iso')
import Data.Traversable.Accum (Accum)
import Prelude (identity)

_Accum :: forall s a. Iso' (Accum s a) { accum :: s, value :: a }
_Accum = identity
