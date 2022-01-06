module Control.Monad.Reader.Trans.Lens where

import Control.Monad.Reader.Trans (ReaderT)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_ReaderT :: forall r m a. Lens' (ReaderT r m a) (r -> m a)
_ReaderT = _Newtype
