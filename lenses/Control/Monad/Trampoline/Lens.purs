module Control.Monad.Trampoline.Lens where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Trampoline (Trampoline)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_Trampoline :: Iso' Trampoline (Free ((->) Unit))
_Trampoline = identity
