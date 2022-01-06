module Data.String.Regex.Flags.Lens where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso (Iso', iso)
import Data.String.Regex.Flags (RegexFlagsRec, RegexFlags(..))
import Prelude (identity)

_RegexFlagsRec :: Iso' RegexFlagsRec
  { global :: Boolean
  , ignoreCase :: Boolean
  , multiline :: Boolean
  , dotAll :: Boolean
  , sticky :: Boolean
  , unicode :: Boolean
  }
_RegexFlagsRec = identity

_RegexFlags :: Lens' RegexFlags RegexFlagsRec
_RegexFlags = iso (\(RegexFlags a) -> a) RegexFlags
