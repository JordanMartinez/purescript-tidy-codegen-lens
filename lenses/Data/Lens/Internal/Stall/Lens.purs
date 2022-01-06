module Data.Lens.Internal.Stall.Lens where

import Data.Either (Either)
import Data.Lens (Lens')
import Data.Lens.Internal.Stall (Stall(..))
import Data.Lens.Iso (iso)
import Data.Tuple (Tuple(..))

_Stall :: forall a b s t. Lens' (Stall a b s t) (Tuple (s -> b -> t) (s -> Either t a))
_Stall = iso (\(Stall a b) -> Tuple a b) \(Tuple a b) -> Stall a b
