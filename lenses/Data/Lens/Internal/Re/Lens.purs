module Data.Lens.Internal.Re.Lens where

import Data.Lens (Lens')
import Data.Lens.Internal.Re (Re)
import Data.Lens.Iso.Newtype (_Newtype)

_Re :: forall p s t a b. Lens' (Re p s t a b) (p b a -> p t s)
_Re = _Newtype
