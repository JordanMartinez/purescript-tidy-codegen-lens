module PureScript.CST.TokenStream.Lens where

import Prelude

import Data.Either (Either(..))
import Data.Lazy (Lazy)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Prism (Prism', prism)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import PureScript.CST.Errors (ParseError)
import PureScript.CST.Layout (LayoutStack)
import PureScript.CST.TokenStream (TokenStream, TokenStep(..))
import PureScript.CST.Types (Comment, LineFeed, SourcePos, SourceToken)

_TokenStream :: Lens' TokenStream (Lazy TokenStep)
_TokenStream = _Newtype

_TokenEOF :: Prism' TokenStep (Tuple SourcePos (Array (Comment LineFeed)))
_TokenEOF = prism (\(Tuple a b) -> TokenEOF a b) case _ of
  TokenEOF a b -> Right (Tuple a b)
  other -> Left other

_TokenError :: Prism' TokenStep
  { arg1 :: SourcePos, arg2 :: ParseError, arg3 :: (Maybe TokenStream), arg4 :: LayoutStack }
_TokenError = prism (\{ arg1, arg2, arg3, arg4 } -> TokenError arg1 arg2 arg3 arg4) case _ of
  TokenError arg1 arg2 arg3 arg4 -> Right { arg1: arg1, arg2: arg2, arg3: arg3, arg4: arg4 }
  other -> Left other

_TokenCons :: Prism' TokenStep
  { arg1 :: SourceToken, arg2 :: SourcePos, arg3 :: TokenStream, arg4 :: LayoutStack }
_TokenCons = prism (\{ arg1, arg2, arg3, arg4 } -> TokenCons arg1 arg2 arg3 arg4) case _ of
  TokenCons arg1 arg2 arg3 arg4 -> Right { arg1: arg1, arg2: arg2, arg3: arg3, arg4: arg4 }
  other -> Left other
