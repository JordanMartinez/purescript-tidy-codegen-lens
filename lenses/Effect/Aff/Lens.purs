module Effect.Aff.Lens where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Effect.Aff (Aff, BracketConditions, Canceler)
import Effect.Exception (Error)
import Prelude (identity)

_Canceler :: Lens' Canceler (Error → Aff Unit)
_Canceler = _Newtype

_BracketConditions
  :: forall a b
   . Iso' (BracketConditions a b)
       { killed ∷ Error → a → Aff Unit
       , failed ∷ Error → a → Aff Unit
       , completed ∷ b → a → Aff Unit
       }
_BracketConditions = identity
