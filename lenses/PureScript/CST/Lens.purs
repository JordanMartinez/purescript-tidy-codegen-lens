module PureScript.CST.Lens where

import Prelude

import Prim hiding (Type)

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Data.Tuple (Tuple(..))
import PureScript.CST (RecoveredParserResult(..))
import PureScript.CST.Parser (Recovered)
import PureScript.CST.Parser.Monad (PositionedError)

_ParseSucceeded :: forall f. Prism' (RecoveredParserResult f) (f Void)
_ParseSucceeded = prism ParseSucceeded case _ of
  ParseSucceeded a -> Right a
  other -> Left other

_ParseSucceededWithErrors
  :: forall f
   . Prism' (RecoveredParserResult f) (Tuple (Recovered f) (NonEmptyArray PositionedError))
_ParseSucceededWithErrors = prism (\(Tuple a b) -> ParseSucceededWithErrors a b) case _ of
  ParseSucceededWithErrors a b -> Right (Tuple a b)
  other -> Left other

_ParseFailed :: forall f. Prism' (RecoveredParserResult f) PositionedError
_ParseFailed = prism ParseFailed case _ of
  ParseFailed a -> Right a
  other -> Left other
