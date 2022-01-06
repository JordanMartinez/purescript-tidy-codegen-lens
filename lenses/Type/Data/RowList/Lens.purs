module Type.Data.RowList.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Prelude (Unit, const, unit)
import Type.Data.RowList (RLProxy(..))

_RLProxy :: forall rowlist. Lens' (RLProxy rowlist) Unit
_RLProxy = iso (const unit) (const RLProxy)
