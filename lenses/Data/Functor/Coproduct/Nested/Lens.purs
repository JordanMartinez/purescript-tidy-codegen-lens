module Data.Functor.Coproduct.Nested.Lens where

import Prelude

import Data.Const (Const)
import Data.Functor.Coproduct (Coproduct)
import Data.Functor.Coproduct.Nested (C10, C11, C2, C3, C4, C5, C6, C7, C8, C9, Coproduct1, Coproduct10, Coproduct2, Coproduct3, Coproduct4, Coproduct5, Coproduct6, Coproduct7, Coproduct8, Coproduct9)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_Coproduct1 :: forall a. Iso' (Coproduct1 a) (C2 a (Const Void))
_Coproduct1 = identity

_Coproduct2 :: forall a b. Iso' (Coproduct2 a b) (C3 a b (Const Void))
_Coproduct2 = identity

_Coproduct3 :: forall a b c. Iso' (Coproduct3 a b c) (C4 a b c (Const Void))
_Coproduct3 = identity

_Coproduct4 :: forall a b c d. Iso' (Coproduct4 a b c d) (C5 a b c d (Const Void))
_Coproduct4 = identity

_Coproduct5 :: forall a b c d e. Iso' (Coproduct5 a b c d e) (C6 a b c d e (Const Void))
_Coproduct5 = identity

_Coproduct6 :: forall a b c d e f. Iso' (Coproduct6 a b c d e f) (C7 a b c d e f (Const Void))
_Coproduct6 = identity

_Coproduct7
  :: forall a b c d e f g. Iso' (Coproduct7 a b c d e f g) (C8 a b c d e f g (Const Void))
_Coproduct7 = identity

_Coproduct8
  :: forall a b c d e f g h. Iso' (Coproduct8 a b c d e f g h) (C9 a b c d e f g h (Const Void))
_Coproduct8 = identity

_Coproduct9
  :: forall a b c d e f g h i
   . Iso' (Coproduct9 a b c d e f g h i) (C10 a b c d e f g h i (Const Void))
_Coproduct9 = identity

_Coproduct10
  :: forall a b c d e f g h i j
   . Iso' (Coproduct10 a b c d e f g h i j) (C11 a b c d e f g h i j (Const Void))
_Coproduct10 = identity

_C2 :: forall a z. Iso' (C2 a z) (Coproduct a z)
_C2 = identity

_C3 :: forall a b z. Iso' (C3 a b z) (Coproduct a (C2 b z))
_C3 = identity

_C4 :: forall a b c z. Iso' (C4 a b c z) (Coproduct a (C3 b c z))
_C4 = identity

_C5 :: forall a b c d z. Iso' (C5 a b c d z) (Coproduct a (C4 b c d z))
_C5 = identity

_C6 :: forall a b c d e z. Iso' (C6 a b c d e z) (Coproduct a (C5 b c d e z))
_C6 = identity

_C7 :: forall a b c d e f z. Iso' (C7 a b c d e f z) (Coproduct a (C6 b c d e f z))
_C7 = identity

_C8 :: forall a b c d e f g z. Iso' (C8 a b c d e f g z) (Coproduct a (C7 b c d e f g z))
_C8 = identity

_C9 :: forall a b c d e f g h z. Iso' (C9 a b c d e f g h z) (Coproduct a (C8 b c d e f g h z))
_C9 = identity

_C10
  :: forall a b c d e f g h i z. Iso' (C10 a b c d e f g h i z) (Coproduct a (C9 b c d e f g h i z))
_C10 = identity

_C11
  :: forall a b c d e f g h i j z
   . Iso' (C11 a b c d e f g h i j z) (Coproduct a (C10 b c d e f g h i j z))
_C11 = identity
