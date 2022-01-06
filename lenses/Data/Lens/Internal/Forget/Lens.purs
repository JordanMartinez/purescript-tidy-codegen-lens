module Data.Lens.Internal.Forget.Lens where

import Data.Lens (Lens')
import Data.Lens.Internal.Forget (Forget)
import Data.Lens.Iso.Newtype (_Newtype)

_Forget :: forall r a b. Lens' (Forget r a b) (a -> r)
_Forget = _Newtype
