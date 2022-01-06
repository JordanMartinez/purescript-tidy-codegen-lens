module Data.Lens.Internal.Focusing.Lens where

import Data.Lens (Lens')
import Data.Lens.Internal.Focusing (Focusing)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Tuple (Tuple)

_Focusing :: forall m s a. Lens' (Focusing m s a) (m (Tuple s a))
_Focusing = _Newtype
