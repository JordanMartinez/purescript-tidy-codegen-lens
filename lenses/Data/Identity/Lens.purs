module Data.Identity.Lens where

import Data.Identity (Identity)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_Identity :: forall a. Lens' (Identity a) a
_Identity = _Newtype
