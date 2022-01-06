module Data.NonEmpty.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.NonEmpty (NonEmpty(..))
import Data.Tuple (Tuple(..))

_NonEmpty :: forall f a. Lens' (NonEmpty f a) (Tuple a (f a))
_NonEmpty = iso (\(NonEmpty a b) -> Tuple a b) \(Tuple a b) -> NonEmpty a b
