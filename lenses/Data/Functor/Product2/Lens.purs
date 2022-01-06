module Data.Functor.Product2.Lens where

import Data.Functor.Product2 (Product2(..))
import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.Tuple (Tuple(..))

_Product2 :: forall f g a b. Lens' (Product2 f g a b) (Tuple (f a b) (g a b))
_Product2 = iso (\(Product2 a b) -> Tuple a b) \(Tuple a b) -> Product2 a b
