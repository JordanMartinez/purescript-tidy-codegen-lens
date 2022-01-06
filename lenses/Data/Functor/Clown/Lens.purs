module Data.Functor.Clown.Lens where

import Data.Functor.Clown (Clown)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_Clown :: forall f a b. Lens' (Clown f a b) (f a)
_Clown = _Newtype
