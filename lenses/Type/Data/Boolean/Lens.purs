module Type.Data.Boolean.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Prelude (Unit, const, unit)
import Type.Data.Boolean (BProxy(..))

_BProxy :: forall bool. Lens' (BProxy bool) Unit
_BProxy = iso (const unit) (const BProxy)
