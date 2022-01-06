module Data.Functor.Product.Nested.Lens where

import Prelude

import Data.Const (Const)
import Data.Functor.Product (Product)
import Data.Functor.Product.Nested (Product1, Product10, Product2, Product3, Product4, Product5, Product6, Product7, Product8, Product9, T10, T11, T2, T3, T4, T5, T6, T7, T8, T9)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_Product1 :: forall a. Iso' (Product1 a) (T2 a (Const Unit))
_Product1 = identity

_Product2 :: forall a b. Iso' (Product2 a b) (T3 a b (Const Unit))
_Product2 = identity

_Product3 :: forall a b c. Iso' (Product3 a b c) (T4 a b c (Const Unit))
_Product3 = identity

_Product4 :: forall a b c d. Iso' (Product4 a b c d) (T5 a b c d (Const Unit))
_Product4 = identity

_Product5 :: forall a b c d e. Iso' (Product5 a b c d e) (T6 a b c d e (Const Unit))
_Product5 = identity

_Product6 :: forall a b c d e f. Iso' (Product6 a b c d e f) (T7 a b c d e f (Const Unit))
_Product6 = identity

_Product7 :: forall a b c d e f g. Iso' (Product7 a b c d e f g) (T8 a b c d e f g (Const Unit))
_Product7 = identity

_Product8
  :: forall a b c d e f g h. Iso' (Product8 a b c d e f g h) (T9 a b c d e f g h (Const Unit))
_Product8 = identity

_Product9
  :: forall a b c d e f g h i
   . Iso' (Product9 a b c d e f g h i) (T10 a b c d e f g h i (Const Unit))
_Product9 = identity

_Product10
  :: forall a b c d e f g h i j
   . Iso' (Product10 a b c d e f g h i j) (T11 a b c d e f g h i j (Const Unit))
_Product10 = identity

_T2 :: forall a z. Iso' (T2 a z) (Product a z)
_T2 = identity

_T3 :: forall a b z. Iso' (T3 a b z) (Product a (T2 b z))
_T3 = identity

_T4 :: forall a b c z. Iso' (T4 a b c z) (Product a (T3 b c z))
_T4 = identity

_T5 :: forall a b c d z. Iso' (T5 a b c d z) (Product a (T4 b c d z))
_T5 = identity

_T6 :: forall a b c d e z. Iso' (T6 a b c d e z) (Product a (T5 b c d e z))
_T6 = identity

_T7 :: forall a b c d e f z. Iso' (T7 a b c d e f z) (Product a (T6 b c d e f z))
_T7 = identity

_T8 :: forall a b c d e f g z. Iso' (T8 a b c d e f g z) (Product a (T7 b c d e f g z))
_T8 = identity

_T9 :: forall a b c d e f g h z. Iso' (T9 a b c d e f g h z) (Product a (T8 b c d e f g h z))
_T9 = identity

_T10
  :: forall a b c d e f g h i z. Iso' (T10 a b c d e f g h i z) (Product a (T9 b c d e f g h i z))
_T10 = identity

_T11
  :: forall a b c d e f g h i j z
   . Iso' (T11 a b c d e f g h i j z) (Product a (T10 b c d e f g h i j z))
_T11 = identity
