module Data.Bifunctor.Join.Lens where

import Data.Bifunctor.Join (Join)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_Join :: forall p a. Lens' (Join p a) (p a a)
_Join = _Newtype
