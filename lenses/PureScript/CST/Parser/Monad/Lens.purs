module PureScript.CST.Parser.Monad.Lens where

import Prelude

import Data.Either (Either(..))
import Data.Lens (Lens')
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (Prism', prism)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Prelude (identity)
import PureScript.CST.Errors (ParseError)
import PureScript.CST.Parser.Monad (ParserState, PositionedError, ParserResult(..), Recovery(..))
import PureScript.CST.TokenStream (TokenStream)
import PureScript.CST.Types (SourcePos)

_PositionedError :: Iso' PositionedError
  { position :: SourcePos
  , error :: ParseError
  }
_PositionedError = identity

_Recovery :: forall a. Lens' (Recovery a) { arg1 :: a, arg2 :: SourcePos, arg3 :: TokenStream }
_Recovery = iso (\(Recovery arg1 arg2 arg3) -> { arg1: arg1, arg2: arg2, arg3: arg3 })
  \{ arg1, arg2, arg3 } -> Recovery arg1 arg2 arg3

_ParseFail
  :: forall a
   . Prism' (ParserResult a)
       { arg1 :: ParseError, arg2 :: SourcePos, arg3 :: ParserState, arg4 :: (Maybe TokenStream) }
_ParseFail = prism (\{ arg1, arg2, arg3, arg4 } -> ParseFail arg1 arg2 arg3 arg4) case _ of
  ParseFail arg1 arg2 arg3 arg4 -> Right { arg1: arg1, arg2: arg2, arg3: arg3, arg4: arg4 }
  other -> Left other

_ParseSucc :: forall a. Prism' (ParserResult a) (Tuple a ParserState)
_ParseSucc = prism (\(Tuple a b) -> ParseSucc a b) case _ of
  ParseSucc a b -> Right (Tuple a b)
  other -> Left other

_ParserState :: Iso' ParserState
  { consumed :: Boolean
  , errors :: Array PositionedError
  , position :: SourcePos
  , stream :: TokenStream
  }
_ParserState = identity
