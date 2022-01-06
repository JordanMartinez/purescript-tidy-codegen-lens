module Data.String.CaseInsensitive.Lens where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.String.CaseInsensitive (CaseInsensitiveString)

_CaseInsensitiveString :: Lens' CaseInsensitiveString String
_CaseInsensitiveString = _Newtype
