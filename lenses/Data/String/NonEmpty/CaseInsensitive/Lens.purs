module Data.String.NonEmpty.CaseInsensitive.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty.CaseInsensitive (CaseInsensitiveNonEmptyString)

_CaseInsensitiveNonEmptyString :: Lens' CaseInsensitiveNonEmptyString NonEmptyString
_CaseInsensitiveNonEmptyString = _Newtype
