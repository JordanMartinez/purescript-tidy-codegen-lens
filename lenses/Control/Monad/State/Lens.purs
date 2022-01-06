module Control.Monad.State.Lens where

import Control.Monad.State (State)
import Control.Monad.State.Trans (StateT)
import Data.Identity (Identity)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_State :: forall s. Iso' (State s) (StateT s Identity)
_State = identity
