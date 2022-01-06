module PureScript.CST.Errors.Lens where

import Prelude

import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Data.Tuple (Tuple(..))
import Prelude (Unit, const, unit)
import PureScript.CST.Errors (ParseError(..))
import PureScript.CST.Types (Token)

_UnexpectedEof :: Prism' ParseError Unit
_UnexpectedEof = prism (const UnexpectedEof) case _ of
  UnexpectedEof -> Right unit
  other -> Left other

_ExpectedEof :: Prism' ParseError Token
_ExpectedEof = prism ExpectedEof case _ of
  ExpectedEof a -> Right a
  other -> Left other

_UnexpectedToken :: Prism' ParseError Token
_UnexpectedToken = prism UnexpectedToken case _ of
  UnexpectedToken a -> Right a
  other -> Left other

_ExpectedToken :: Prism' ParseError (Tuple Token Token)
_ExpectedToken = prism (\(Tuple a b) -> ExpectedToken a b) case _ of
  ExpectedToken a b -> Right (Tuple a b)
  other -> Left other

_ExpectedClass :: Prism' ParseError (Tuple String Token)
_ExpectedClass = prism (\(Tuple a b) -> ExpectedClass a b) case _ of
  ExpectedClass a b -> Right (Tuple a b)
  other -> Left other

_LexExpected :: Prism' ParseError (Tuple String String)
_LexExpected = prism (\(Tuple a b) -> LexExpected a b) case _ of
  LexExpected a b -> Right (Tuple a b)
  other -> Left other

_LexInvalidCharEscape :: Prism' ParseError String
_LexInvalidCharEscape = prism LexInvalidCharEscape case _ of
  LexInvalidCharEscape a -> Right a
  other -> Left other

_LexCharEscapeOutOfRange :: Prism' ParseError String
_LexCharEscapeOutOfRange = prism LexCharEscapeOutOfRange case _ of
  LexCharEscapeOutOfRange a -> Right a
  other -> Left other

_LexHexOutOfRange :: Prism' ParseError String
_LexHexOutOfRange = prism LexHexOutOfRange case _ of
  LexHexOutOfRange a -> Right a
  other -> Left other

_LexIntOutOfRange :: Prism' ParseError String
_LexIntOutOfRange = prism LexIntOutOfRange case _ of
  LexIntOutOfRange a -> Right a
  other -> Left other

_LexNumberOutOfRange :: Prism' ParseError String
_LexNumberOutOfRange = prism LexNumberOutOfRange case _ of
  LexNumberOutOfRange a -> Right a
  other -> Left other
