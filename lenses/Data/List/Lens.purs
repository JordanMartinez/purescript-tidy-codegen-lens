module Data.List.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (Pattern)
import Data.List.Types (List)

_Pattern :: forall a. Lens' (Pattern a) (List a)
_Pattern = _Newtype
