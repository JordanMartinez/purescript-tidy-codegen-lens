module Control.Comonad.Traced.Trans.Lens where

import Control.Comonad.Traced.Trans (TracedT)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_TracedT :: forall t w a. Lens' (TracedT t w a) (w (t -> a))
_TracedT = _Newtype
