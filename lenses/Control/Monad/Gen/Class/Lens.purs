module Control.Monad.Gen.Class.Lens where

import Prelude

import Control.Monad.Gen.Class (Size)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_Size :: Iso' Size Int
_Size = identity
