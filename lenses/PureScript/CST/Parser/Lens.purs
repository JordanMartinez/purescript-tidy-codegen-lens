module PureScript.CST.Parser.Lens where

import Data.Lens.Iso (Iso')
import Prelude (identity)
import PureScript.CST.Errors (RecoveredError)
import PureScript.CST.Parser (Recovered)

_Recovered :: forall f. Iso' (Recovered f) (f RecoveredError)
_Recovered = identity
