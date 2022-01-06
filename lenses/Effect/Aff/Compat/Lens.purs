module Effect.Aff.Compat.Lens where

import Prelude

import Data.Lens.Iso (Iso')
import Effect.Aff.Compat (EffectFnCb)
import Effect.Uncurried (EffectFn1)
import Prelude (identity)

_EffectFnCb :: forall a. Iso' (EffectFnCb a) (EffectFn1 a Unit)
_EffectFnCb = identity
