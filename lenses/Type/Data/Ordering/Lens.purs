module Type.Data.Ordering.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Prelude (Unit, const, unit)
import Type.Data.Ordering (OProxy(..))

_OProxy :: forall ordering. Lens' (OProxy ordering) Unit
_OProxy = iso (const unit) (const OProxy)
