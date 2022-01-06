module Data.List.Lazy.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List.Lazy (Pattern)
import Data.List.Lazy.Types (List)

_Pattern :: forall a. Lens' (Pattern a) (List a)
_Pattern = _Newtype
