module Data.Tuple.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.Tuple (Tuple(..))

_Tuple :: forall a b. Lens' (Tuple a b) (Tuple a b)
_Tuple = iso (\(Tuple a b) -> Tuple a b) \(Tuple a b) -> Tuple a b
