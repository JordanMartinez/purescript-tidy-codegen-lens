module Data.Op.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Op (Op)

_Op :: forall a b. Lens' (Op a b) (b -> a)
_Op = _Newtype
