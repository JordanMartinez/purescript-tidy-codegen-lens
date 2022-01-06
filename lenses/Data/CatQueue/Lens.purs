module Data.CatQueue.Lens where

import Data.CatQueue (CatQueue(..))
import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.List (List)
import Data.Tuple (Tuple(..))

_CatQueue :: forall a. Lens' (CatQueue a) (Tuple (List a) (List a))
_CatQueue = iso (\(CatQueue a b) -> Tuple a b) \(Tuple a b) -> CatQueue a b
