module Control.Monad.Identity.Trans.Lens where

import Control.Monad.Identity.Trans (IdentityT)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_IdentityT :: forall m a. Lens' (IdentityT m a) (m a)
_IdentityT = _Newtype
