module Data.Ordering.Lens where

import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Data.Ordering (Ordering(..))
import Prelude (Unit, const, unit)

_LT :: Prism' Ordering Unit
_LT = prism (const LT) case _ of
  LT -> Right unit
  other -> Left other

_GT :: Prism' Ordering Unit
_GT = prism (const GT) case _ of
  GT -> Right unit
  other -> Left other

_EQ :: Prism' Ordering Unit
_EQ = prism (const EQ) case _ of
  EQ -> Right unit
  other -> Left other
