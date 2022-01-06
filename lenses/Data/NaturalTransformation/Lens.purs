module Data.NaturalTransformation.Lens where

import Data.Lens.Iso (Iso')
import Data.NaturalTransformation (NaturalTransformation)
import Prelude (identity)

_NaturalTransformation :: forall f g. Iso' (NaturalTransformation f g) (forall a. f a -> g a)
_NaturalTransformation = identity
