module Data.Lens.Internal.Zipping.Lens where

import Data.Lens (Lens')
import Data.Lens.Internal.Zipping (Zipping)
import Data.Lens.Iso.Newtype (_Newtype)

_Zipping :: forall a b. Lens' (Zipping a b) (a -> a -> b)
_Zipping = _Newtype
