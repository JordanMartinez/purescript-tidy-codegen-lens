module Control.Monad.State.Trans.Lens where

import Control.Monad.State.Trans (StateT)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Tuple (Tuple)

_StateT :: forall s m a. Lens' (StateT s m a) (s -> m (Tuple a s))
_StateT = _Newtype
