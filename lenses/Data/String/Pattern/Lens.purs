module Data.String.Pattern.Lens where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.String.Pattern (Pattern, Replacement)

_Pattern :: Lens' Pattern String
_Pattern = _Newtype

_Replacement :: Lens' Replacement String
_Replacement = _Newtype
