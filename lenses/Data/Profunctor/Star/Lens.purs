module Data.Profunctor.Star.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Profunctor.Star (Star)

_Star :: forall f a b. Lens' (Star f a b) (a -> f b)
_Star = _Newtype
