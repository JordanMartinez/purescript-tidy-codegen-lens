module Control.Monad.Except.Trans.Lens where

import Control.Monad.Except.Trans (ExceptT)
import Data.Either (Either)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_ExceptT :: forall e m a. Lens' (ExceptT e m a) (m (Either e a))
_ExceptT = _Newtype
