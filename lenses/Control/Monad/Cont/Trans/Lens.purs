module Control.Monad.Cont.Trans.Lens where

import Control.Monad.Cont.Trans (ContT)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_ContT :: forall r m a. Lens' (ContT r m a) ((a -> m r) -> m r)
_ContT = _Newtype
