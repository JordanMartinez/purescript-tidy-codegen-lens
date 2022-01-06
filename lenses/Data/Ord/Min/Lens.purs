module Data.Ord.Min.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Ord.Min (Min)

_Min :: forall a. Lens' (Min a) a
_Min = _Newtype
