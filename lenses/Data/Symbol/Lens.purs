module Data.Symbol.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.Symbol (SProxy(..))
import Prelude (Unit, const, unit)

_SProxy :: forall sym. Lens' (SProxy sym) Unit
_SProxy = iso (const unit) (const SProxy)
