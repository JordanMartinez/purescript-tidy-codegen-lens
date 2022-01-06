module Tidy.Doc.Lens where

import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Prelude (Unit, const, unit)
import Tidy.Doc (ForceBreak(..))

_ForceNone :: Prism' ForceBreak Unit
_ForceNone = prism (const ForceNone) case _ of
  ForceNone -> Right unit
  other -> Left other

_ForceSpace :: Prism' ForceBreak Unit
_ForceSpace = prism (const ForceSpace) case _ of
  ForceSpace -> Right unit
  other -> Left other

_ForceBreak :: Prism' ForceBreak Unit
_ForceBreak = prism (const ForceBreak) case _ of
  ForceBreak -> Right unit
  other -> Left other
