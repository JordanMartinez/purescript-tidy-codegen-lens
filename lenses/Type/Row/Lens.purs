module Type.Row.Lens where

import Data.Lens.Iso (Iso')
import Prelude (identity)
import Type.Row (RowApply)

_RowApply :: forall f a. Iso' (RowApply f a) (f a)
_RowApply = identity
