module Foreign.Lens where

import Prelude

import Control.Monad.Except (Except, ExceptT)
import Data.Either (Either(..))
import Data.Lens.Iso (Iso')
import Data.Lens.Prism (Prism', prism)
import Data.List.NonEmpty (NonEmptyList)
import Data.Tuple (Tuple(..))
import Foreign (F, FT, MultipleErrors, ForeignError(..))
import Prelude (identity)

_ForeignError :: Prism' ForeignError String
_ForeignError = prism ForeignError case _ of
  ForeignError a -> Right a
  other -> Left other

_TypeMismatch :: Prism' ForeignError (Tuple String String)
_TypeMismatch = prism (\(Tuple a b) -> TypeMismatch a b) case _ of
  TypeMismatch a b -> Right (Tuple a b)
  other -> Left other

_ErrorAtIndex :: Prism' ForeignError (Tuple Int ForeignError)
_ErrorAtIndex = prism (\(Tuple a b) -> ErrorAtIndex a b) case _ of
  ErrorAtIndex a b -> Right (Tuple a b)
  other -> Left other

_ErrorAtProperty :: Prism' ForeignError (Tuple String ForeignError)
_ErrorAtProperty = prism (\(Tuple a b) -> ErrorAtProperty a b) case _ of
  ErrorAtProperty a b -> Right (Tuple a b)
  other -> Left other

_MultipleErrors :: Iso' MultipleErrors (NonEmptyList ForeignError)
_MultipleErrors = identity

_F :: Iso' F (Except MultipleErrors)
_F = identity

_FT :: Iso' FT (ExceptT MultipleErrors)
_FT = identity
