module Data.Lens.Internal.Tagged.Lens where

import Data.Lens (Lens')
import Data.Lens.Internal.Tagged (Tagged)
import Data.Lens.Iso.Newtype (_Newtype)

_Tagged :: forall a b. Lens' (Tagged a b) b
_Tagged = _Newtype
