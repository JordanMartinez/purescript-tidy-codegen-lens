module Data.Lens.Internal.Market.Lens where

import Data.Either (Either)
import Data.Lens (Lens')
import Data.Lens.Internal.Market (Market(..))
import Data.Lens.Iso (iso)
import Data.Tuple (Tuple(..))

_Market :: forall a b s t. Lens' (Market a b s t) (Tuple (b -> t) (s -> Either t a))
_Market = iso (\(Market a b) -> Tuple a b) \(Tuple a b) -> Market a b
