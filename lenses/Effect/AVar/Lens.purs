module Effect.AVar.Lens where

import Prelude

import Data.Either (Either(..))
import Data.Lens.Iso (Iso')
import Data.Lens.Prism (Prism', prism)
import Effect (Effect)
import Effect.AVar (AVarCallback, AVarStatus(..))
import Effect.Exception (Error)
import Prelude (Unit, const, identity, unit)

_AVarCallback :: forall a. Iso' (AVarCallback a) (Either Error a → Effect Unit)
_AVarCallback = identity

_Killed :: forall a. Prism' (AVarStatus a) Error
_Killed = prism Killed case _ of
  Killed a -> Right a
  other -> Left other

_Filled :: forall a. Prism' (AVarStatus a) a
_Filled = prism Filled case _ of
  Filled a -> Right a
  other -> Left other

_Empty :: forall a. Prism' (AVarStatus a) Unit
_Empty = prism (const Empty) case _ of
  Empty -> Right unit
  other -> Left other
