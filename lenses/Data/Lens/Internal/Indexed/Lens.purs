module Data.Lens.Internal.Indexed.Lens where

import Data.Lens (Lens')
import Data.Lens.Internal.Indexed (Indexed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Tuple (Tuple)

_Indexed :: forall p i s t. Lens' (Indexed p i s t) (p (Tuple i s) t)
_Indexed = _Newtype
