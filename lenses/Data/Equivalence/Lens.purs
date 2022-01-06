module Data.Equivalence.Lens where

import Prelude

import Data.Equivalence (Equivalence)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_Equivalence :: forall a. Lens' (Equivalence a) (a -> a -> Boolean)
_Equivalence = _Newtype
