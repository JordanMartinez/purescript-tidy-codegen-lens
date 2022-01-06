module Control.Parallel.Class.Lens where

import Prelude

import Control.Monad.Cont.Trans (ContT)
import Control.Parallel.Class (ParCont)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_ParCont :: forall m a. Lens' (ParCont m a) (ContT Unit m a)
_ParCont = _Newtype
