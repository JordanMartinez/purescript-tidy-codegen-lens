module Data.Function.Uncurried.Lens where

import Data.Function.Uncurried (Fn1)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_Fn1 :: forall a b. Iso' (Fn1 a b) (a -> b)
_Fn1 = identity
