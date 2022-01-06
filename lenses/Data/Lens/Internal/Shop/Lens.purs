module Data.Lens.Internal.Shop.Lens where

import Data.Lens (Lens')
import Data.Lens.Internal.Shop (Shop(..))
import Data.Lens.Iso (iso)
import Data.Tuple (Tuple(..))

_Shop :: forall a b s t. Lens' (Shop a b s t) (Tuple (s -> a) (s -> b -> t))
_Shop = iso (\(Shop a b) -> Tuple a b) \(Tuple a b) -> Shop a b
