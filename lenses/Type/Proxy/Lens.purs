module Type.Proxy.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Prelude (Unit, const, unit)
import Type.Proxy (Proxy(..), Proxy2(..), Proxy3(..))

_Proxy :: forall a. Lens' (Proxy a) Unit
_Proxy = iso (const unit) (const Proxy)

_Proxy2 :: forall f. Lens' (Proxy2 f) Unit
_Proxy2 = iso (const unit) (const Proxy2)

_Proxy3 :: forall a. Lens' (Proxy3 a) Unit
_Proxy3 = iso (const unit) (const Proxy3)
