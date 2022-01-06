module Control.Comonad.Traced.Lens where

import Control.Comonad.Traced (Traced)
import Control.Comonad.Traced.Trans (TracedT)
import Data.Identity (Identity)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_Traced :: forall m. Iso' (Traced m) (TracedT m Identity)
_Traced = identity
