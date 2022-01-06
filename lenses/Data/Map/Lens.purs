module Data.Map.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map (SemigroupMap)
import Data.Map.Internal (Map)

_SemigroupMap :: forall k v. Lens' (SemigroupMap k v) (Map k v)
_SemigroupMap = _Newtype
