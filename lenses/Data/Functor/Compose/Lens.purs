module Data.Functor.Compose.Lens where

import Data.Functor.Compose (Compose)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_Compose :: forall f g a. Lens' (Compose f g a) (f (g a))
_Compose = _Newtype
