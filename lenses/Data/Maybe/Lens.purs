module Data.Maybe.Lens where

import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Data.Maybe (Maybe(..))
import Prelude (Unit, const, unit)

_Nothing :: forall a. Prism' (Maybe a) Unit
_Nothing = prism (const Nothing) case _ of
  Nothing -> Right unit
  other -> Left other

_Just :: forall a. Prism' (Maybe a) a
_Just = prism Just case _ of
  Just a -> Right a
  other -> Left other
