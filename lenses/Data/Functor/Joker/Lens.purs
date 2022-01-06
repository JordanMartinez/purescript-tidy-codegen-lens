module Data.Functor.Joker.Lens where

import Data.Functor.Joker (Joker)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_Joker :: forall g a b. Lens' (Joker g a b) (g b)
_Joker = _Newtype
