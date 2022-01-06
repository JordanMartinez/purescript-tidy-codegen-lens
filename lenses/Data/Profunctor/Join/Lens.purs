module Data.Profunctor.Join.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Profunctor.Join (Join)

_Join :: forall p a. Lens' (Join p a) (p a a)
_Join = _Newtype
