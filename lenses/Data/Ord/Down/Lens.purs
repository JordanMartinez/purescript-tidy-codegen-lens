module Data.Ord.Down.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Ord.Down (Down)

_Down :: forall a. Lens' (Down a) a
_Down = _Newtype
