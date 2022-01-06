module Control.Monad.Cont.Lens where

import Control.Monad.Cont (Cont)
import Control.Monad.Cont.Trans (ContT)
import Data.Identity (Identity)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_Cont :: forall r. Iso' (Cont r) (ContT r Identity)
_Cont = identity
