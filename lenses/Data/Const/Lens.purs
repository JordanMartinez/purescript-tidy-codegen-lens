module Data.Const.Lens where

import Data.Const (Const)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_Const :: forall a b. Lens' (Const a b) a
_Const = _Newtype
