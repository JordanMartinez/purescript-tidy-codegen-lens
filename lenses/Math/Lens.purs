module Math.Lens where

import Data.Lens.Iso (Iso')
import Math (Radians)
import Prelude (identity)

_Radians :: Iso' Radians Number
_Radians = identity
