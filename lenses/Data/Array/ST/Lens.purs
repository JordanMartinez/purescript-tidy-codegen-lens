module Data.Array.ST.Lens where

import Prelude

import Data.Array.ST (Assoc)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_Assoc :: forall a. Iso' (Assoc a) { value :: a, index :: Int }
_Assoc = identity
