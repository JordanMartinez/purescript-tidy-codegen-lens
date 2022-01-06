module Data.Functor.Flip.Lens where

import Data.Functor.Flip (Flip)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_Flip :: forall p a b. Lens' (Flip p a b) (p b a)
_Flip = _Newtype
