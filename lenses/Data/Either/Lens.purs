module Data.Either.Lens where

import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)

_Left :: forall a b. Prism' (Either a b) a
_Left = prism Left case _ of
  Left a -> Right a
  other -> Left other

_Right :: forall a b. Prism' (Either a b) b
_Right = prism Right case _ of
  Right a -> Right a
  other -> Left other
