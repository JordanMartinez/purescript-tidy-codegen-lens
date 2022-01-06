module Data.Tuple.Nested.Lens where

import Prelude

import Data.Lens.Iso (Iso')
import Data.Tuple (Tuple)
import Data.Tuple.Nested (T10, T11, T2, T3, T4, T5, T6, T7, T8, T9, Tuple1, Tuple10, Tuple2, Tuple3, Tuple4, Tuple5, Tuple6, Tuple7, Tuple8, Tuple9)
import Prelude (identity)

_Tuple1 :: forall a. Iso' (Tuple1 a) (T2 a Unit)
_Tuple1 = identity

_Tuple2 :: forall a b. Iso' (Tuple2 a b) (T3 a b Unit)
_Tuple2 = identity

_Tuple3 :: forall a b c. Iso' (Tuple3 a b c) (T4 a b c Unit)
_Tuple3 = identity

_Tuple4 :: forall a b c d. Iso' (Tuple4 a b c d) (T5 a b c d Unit)
_Tuple4 = identity

_Tuple5 :: forall a b c d e. Iso' (Tuple5 a b c d e) (T6 a b c d e Unit)
_Tuple5 = identity

_Tuple6 :: forall a b c d e f. Iso' (Tuple6 a b c d e f) (T7 a b c d e f Unit)
_Tuple6 = identity

_Tuple7 :: forall a b c d e f g. Iso' (Tuple7 a b c d e f g) (T8 a b c d e f g Unit)
_Tuple7 = identity

_Tuple8 :: forall a b c d e f g h. Iso' (Tuple8 a b c d e f g h) (T9 a b c d e f g h Unit)
_Tuple8 = identity

_Tuple9 :: forall a b c d e f g h i. Iso' (Tuple9 a b c d e f g h i) (T10 a b c d e f g h i Unit)
_Tuple9 = identity

_Tuple10
  :: forall a b c d e f g h i j. Iso' (Tuple10 a b c d e f g h i j) (T11 a b c d e f g h i j Unit)
_Tuple10 = identity

_T2 :: forall a z. Iso' (T2 a z) (Tuple a z)
_T2 = identity

_T3 :: forall a b z. Iso' (T3 a b z) (Tuple a (T2 b z))
_T3 = identity

_T4 :: forall a b c z. Iso' (T4 a b c z) (Tuple a (T3 b c z))
_T4 = identity

_T5 :: forall a b c d z. Iso' (T5 a b c d z) (Tuple a (T4 b c d z))
_T5 = identity

_T6 :: forall a b c d e z. Iso' (T6 a b c d e z) (Tuple a (T5 b c d e z))
_T6 = identity

_T7 :: forall a b c d e f z. Iso' (T7 a b c d e f z) (Tuple a (T6 b c d e f z))
_T7 = identity

_T8 :: forall a b c d e f g z. Iso' (T8 a b c d e f g z) (Tuple a (T7 b c d e f g z))
_T8 = identity

_T9 :: forall a b c d e f g h z. Iso' (T9 a b c d e f g h z) (Tuple a (T8 b c d e f g h z))
_T9 = identity

_T10 :: forall a b c d e f g h i z. Iso' (T10 a b c d e f g h i z) (Tuple a (T9 b c d e f g h i z))
_T10 = identity

_T11
  :: forall a b c d e f g h i j z
   . Iso' (T11 a b c d e f g h i j z) (Tuple a (T10 b c d e f g h i j z))
_T11 = identity
