module Type.Data.Row.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Prelude (Unit, const, unit)
import Type.Data.Row (RProxy(..))

_RProxy :: forall row. Lens' (RProxy row) Unit
_RProxy = iso (const unit) (const RProxy)
