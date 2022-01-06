module Data.Enum.Lens where

import Prelude

import Data.Enum (Cardinality)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_Cardinality :: forall a. Lens' (Cardinality a) Int
_Cardinality = _Newtype
