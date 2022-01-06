module Data.Functor.Costar.Lens where

import Data.Functor.Costar (Costar)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_Costar :: forall f b a. Lens' (Costar f b a) (f b -> a)
_Costar = _Newtype
