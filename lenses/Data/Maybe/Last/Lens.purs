module Data.Maybe.Last.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe)
import Data.Maybe.Last (Last)

_Last :: forall a. Lens' (Last a) (Maybe a)
_Last = _Newtype
