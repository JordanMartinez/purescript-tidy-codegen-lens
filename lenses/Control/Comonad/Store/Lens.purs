module Control.Comonad.Store.Lens where

import Control.Comonad.Store (Store)
import Control.Comonad.Store.Trans (StoreT)
import Data.Identity (Identity)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_Store :: forall s. Iso' (Store s) (StoreT s Identity)
_Store = identity
