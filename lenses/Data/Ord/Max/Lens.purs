module Data.Ord.Max.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Ord.Max (Max)

_Max :: forall a. Lens' (Max a) a
_Max = _Newtype
