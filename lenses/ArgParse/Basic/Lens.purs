module ArgParse.Basic.Lens where

import Prelude

import ArgParse.Basic (ArgError(..), ArgErrorMsg(..), ArgHelp(..))
import Data.Either (Either(..))
import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.Lens.Prism (Prism', prism)
import Data.List (List)
import Data.Tuple (Tuple(..))
import Prelude (Unit, const, unit)

_HelpFlag :: Prism' ArgHelp (Tuple (Array String) String)
_HelpFlag = prism (\(Tuple a b) -> HelpFlag a b) case _ of
  HelpFlag a b -> Right (Tuple a b)
  other -> Left other

_HelpAny :: Prism' ArgHelp String
_HelpAny = prism HelpAny case _ of
  HelpAny a -> Right a
  other -> Left other

_HelpFormat :: Prism' ArgHelp (Tuple String ArgHelp)
_HelpFormat = prism (\(Tuple a b) -> HelpFormat a b) case _ of
  HelpFormat a b -> Right (Tuple a b)
  other -> Left other

_HelpArgs :: Prism' ArgHelp (Array ArgHelp)
_HelpArgs = prism HelpArgs case _ of
  HelpArgs a -> Right a
  other -> Left other

_HelpChoose :: Prism' ArgHelp (Tuple String (Array ArgHelp))
_HelpChoose = prism (\(Tuple a b) -> HelpChoose a b) case _ of
  HelpChoose a b -> Right (Tuple a b)
  other -> Left other

_HelpCommand :: Prism' ArgHelp { arg1 :: (Array String), arg2 :: String, arg3 :: ArgHelp }
_HelpCommand = prism (\{ arg1, arg2, arg3 } -> HelpCommand arg1 arg2 arg3) case _ of
  HelpCommand arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_HelpRest :: Prism' ArgHelp String
_HelpRest = prism HelpRest case _ of
  HelpRest a -> Right a
  other -> Left other

_ArgError :: Lens' ArgError (Tuple (List ArgHelp) ArgErrorMsg)
_ArgError = iso (\(ArgError a b) -> Tuple a b) \(Tuple a b) -> ArgError a b

_ExpectedFlag :: Prism' ArgErrorMsg Unit
_ExpectedFlag = prism (const ExpectedFlag) case _ of
  ExpectedFlag -> Right unit
  other -> Left other

_ExpectedArgValue :: Prism' ArgErrorMsg Unit
_ExpectedArgValue = prism (const ExpectedArgValue) case _ of
  ExpectedArgValue -> Right unit
  other -> Left other

_ExpectedRest :: Prism' ArgErrorMsg Unit
_ExpectedRest = prism (const ExpectedRest) case _ of
  ExpectedRest -> Right unit
  other -> Left other

_ExpectedArg :: Prism' ArgErrorMsg Unit
_ExpectedArg = prism (const ExpectedArg) case _ of
  ExpectedArg -> Right unit
  other -> Left other

_DuplicateArg :: Prism' ArgErrorMsg Unit
_DuplicateArg = prism (const DuplicateArg) case _ of
  DuplicateArg -> Right unit
  other -> Left other

_UnformatFailed :: Prism' ArgErrorMsg String
_UnformatFailed = prism UnformatFailed case _ of
  UnformatFailed a -> Right a
  other -> Left other

_UnknownArg :: Prism' ArgErrorMsg String
_UnknownArg = prism UnknownArg case _ of
  UnknownArg a -> Right a
  other -> Left other

_ShowHelp :: Prism' ArgErrorMsg Unit
_ShowHelp = prism (const ShowHelp) case _ of
  ShowHelp -> Right unit
  other -> Left other

_ShowInfo :: Prism' ArgErrorMsg String
_ShowInfo = prism ShowInfo case _ of
  ShowInfo a -> Right a
  other -> Left other
