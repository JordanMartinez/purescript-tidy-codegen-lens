module Data.Either.Nested.Lens where

import Data.Either.Nested (Either1, Either10, Either2, Either3, Either4, Either5, Either6, Either7, Either8, Either9, type (\/))
import Data.Lens.Iso (Iso')
import Data.Void (Void)
import Prelude (identity)

_Either1 :: forall a. Iso' (Either1 a) (a \/ Void)
_Either1 = identity

_Either2 :: forall a b. Iso' (Either2 a b) (a \/ b \/ Void)
_Either2 = identity

_Either3 :: forall a b c. Iso' (Either3 a b c) (a \/ b \/ c \/ Void)
_Either3 = identity

_Either4 :: forall a b c d. Iso' (Either4 a b c d) (a \/ b \/ c \/ d \/ Void)
_Either4 = identity

_Either5 :: forall a b c d e. Iso' (Either5 a b c d e) (a \/ b \/ c \/ d \/ e \/ Void)
_Either5 = identity

_Either6 :: forall a b c d e f. Iso' (Either6 a b c d e f) (a \/ b \/ c \/ d \/ e \/ f \/ Void)
_Either6 = identity

_Either7
  :: forall a b c d e f g. Iso' (Either7 a b c d e f g) (a \/ b \/ c \/ d \/ e \/ f \/ g \/ Void)
_Either7 = identity

_Either8
  :: forall a b c d e f g h
   . Iso' (Either8 a b c d e f g h) (a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ Void)
_Either8 = identity

_Either9
  :: forall a b c d e f g h i
   . Iso' (Either9 a b c d e f g h i) (a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ Void)
_Either9 = identity

_Either10
  :: forall a b c d e f g h i j
   . Iso' (Either10 a b c d e f g h i j) (a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ j \/ Void)
_Either10 = identity
