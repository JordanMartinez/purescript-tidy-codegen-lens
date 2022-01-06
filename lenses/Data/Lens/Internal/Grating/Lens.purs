module Data.Lens.Internal.Grating.Lens where

import Data.Lens (Lens')
import Data.Lens.Internal.Grating (Grating)
import Data.Lens.Iso.Newtype (_Newtype)

_Grating :: forall a b s t. Lens' (Grating a b s t) (((s -> a) -> b) -> t)
_Grating = _Newtype
