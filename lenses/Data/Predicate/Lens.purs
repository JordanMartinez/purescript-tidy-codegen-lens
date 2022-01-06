module Data.Predicate.Lens where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Predicate (Predicate)

_Predicate :: forall a. Lens' (Predicate a) (a -> Boolean)
_Predicate = _Newtype
