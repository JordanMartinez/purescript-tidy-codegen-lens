module Control.Comonad.Env.Lens where

import Control.Comonad.Env (Env)
import Control.Comonad.Env.Trans (EnvT)
import Data.Identity (Identity)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_Env :: forall e. Iso' (Env e) (EnvT e Identity)
_Env = identity
