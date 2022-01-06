module Control.Monad.Maybe.Trans.Lens where

import Control.Monad.Maybe.Trans (MaybeT)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe)

_MaybeT :: forall m a. Lens' (MaybeT m a) (m (Maybe a))
_MaybeT = _Newtype
