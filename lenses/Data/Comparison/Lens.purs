module Data.Comparison.Lens where

import Prelude

import Data.Comparison (Comparison)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_Comparison :: forall a. Lens' (Comparison a) (a -> a -> Ordering)
_Comparison = _Newtype
