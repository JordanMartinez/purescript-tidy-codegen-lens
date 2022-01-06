module Type.Function.Lens where

import Data.Lens.Iso (Iso')
import Prelude (identity)
import Type.Function (APPLY, FLIP)

_APPLY :: forall f a. Iso' (APPLY f a) (f a)
_APPLY = identity

_FLIP :: forall a f. Iso' (FLIP a f) (f a)
_FLIP = identity
