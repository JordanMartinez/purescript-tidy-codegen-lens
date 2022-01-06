module Data.Functor.Coproduct.Lens where

import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_Coproduct :: forall f g a. Lens' (Coproduct f g a) (Either (f a) (g a))
_Coproduct = _Newtype
