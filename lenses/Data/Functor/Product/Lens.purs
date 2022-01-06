module Data.Functor.Product.Lens where

import Data.Functor.Product (Product)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Tuple (Tuple)

_Product :: forall f g a. Lens' (Product f g a) (Tuple (f a) (g a))
_Product = _Newtype
