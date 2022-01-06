module Control.Monad.Except.Lens where

import Control.Monad.Except (Except)
import Control.Monad.Except.Trans (ExceptT)
import Data.Identity (Identity)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_Except :: forall e. Iso' (Except e) (ExceptT e Identity)
_Except = identity
