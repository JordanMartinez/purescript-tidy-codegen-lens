module Data.Interval.Duration.Iso.Lens where

import Data.Either (Either(..))
import Data.Interval.Duration (DurationComponent)
import Data.Interval.Duration.Iso (Errors, Error(..))
import Data.Lens.Iso (Iso')
import Data.Lens.Prism (Prism', prism)
import Data.List.Types (NonEmptyList)
import Prelude (Unit, const, identity, unit)

_Errors :: Iso' Errors (NonEmptyList Error)
_Errors = identity

_IsEmpty :: Prism' Error Unit
_IsEmpty = prism (const IsEmpty) case _ of
  IsEmpty -> Right unit
  other -> Left other

_InvalidWeekComponentUsage :: Prism' Error Unit
_InvalidWeekComponentUsage = prism (const InvalidWeekComponentUsage) case _ of
  InvalidWeekComponentUsage -> Right unit
  other -> Left other

_ContainsNegativeValue :: Prism' Error DurationComponent
_ContainsNegativeValue = prism ContainsNegativeValue case _ of
  ContainsNegativeValue a -> Right a
  other -> Left other

_InvalidFractionalUse :: Prism' Error DurationComponent
_InvalidFractionalUse = prism InvalidFractionalUse case _ of
  InvalidFractionalUse a -> Right a
  other -> Left other
