module Tidy.Token.Lens where

import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Prelude (Unit, const, unit)
import Tidy.Token (UnicodeOption(..))

_UnicodeSource :: Prism' UnicodeOption Unit
_UnicodeSource = prism (const UnicodeSource) case _ of
  UnicodeSource -> Right unit
  other -> Left other

_UnicodeAlways :: Prism' UnicodeOption Unit
_UnicodeAlways = prism (const UnicodeAlways) case _ of
  UnicodeAlways -> Right unit
  other -> Left other

_UnicodeNever :: Prism' UnicodeOption Unit
_UnicodeNever = prism (const UnicodeNever) case _ of
  UnicodeNever -> Right unit
  other -> Left other
