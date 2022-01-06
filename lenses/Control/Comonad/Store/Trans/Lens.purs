module Control.Comonad.Store.Trans.Lens where

import Control.Comonad.Store.Trans (StoreT)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Tuple (Tuple)

_StoreT :: forall s w a. Lens' (StoreT s w a) (Tuple (w (s -> a)) s)
_StoreT = _Newtype
