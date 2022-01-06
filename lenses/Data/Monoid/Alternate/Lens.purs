module Data.Monoid.Alternate.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Monoid.Alternate (Alternate)

_Alternate :: forall f a. Lens' (Alternate f a) (f a)
_Alternate = _Newtype
