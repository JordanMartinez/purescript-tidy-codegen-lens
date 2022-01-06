module Control.Comonad.Env.Trans.Lens where

import Control.Comonad.Env.Trans (EnvT)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Tuple (Tuple)

_EnvT :: forall e w a. Lens' (EnvT e w a) (Tuple e (w a))
_EnvT = _Newtype
