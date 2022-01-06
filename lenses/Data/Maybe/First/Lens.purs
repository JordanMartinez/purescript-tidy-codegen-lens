module Data.Maybe.First.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe)
import Data.Maybe.First (First)

_First :: forall a. Lens' (First a) (Maybe a)
_First = _Newtype
