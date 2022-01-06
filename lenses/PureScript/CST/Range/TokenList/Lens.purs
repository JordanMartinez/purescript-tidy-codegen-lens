module PureScript.CST.Range.TokenList.Lens where

import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Data.Tuple (Tuple(..))
import Prelude (Unit, const, unit)
import PureScript.CST.Range.TokenList (TokenList, UnconsToken(..))
import PureScript.CST.Types (SourceToken)

_UnconsDone :: Prism' UnconsToken Unit
_UnconsDone = prism (const UnconsDone) case _ of
  UnconsDone -> Right unit
  other -> Left other

_UnconsMore :: Prism' UnconsToken (Tuple SourceToken TokenList)
_UnconsMore = prism (\(Tuple a b) -> UnconsMore a b) case _ of
  UnconsMore a b -> Right (Tuple a b)
  other -> Left other
