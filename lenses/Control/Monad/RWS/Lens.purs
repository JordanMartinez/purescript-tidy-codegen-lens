module Control.Monad.RWS.Lens where

import Control.Monad.RWS (RWS)
import Control.Monad.RWS.Trans (RWST)
import Data.Identity (Identity)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_RWS :: forall r w s. Iso' (RWS r w s) (RWST r w s Identity)
_RWS = identity
