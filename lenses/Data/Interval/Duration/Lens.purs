module Data.Interval.Duration.Lens where

import Prelude

import Data.Either (Either(..))
import Data.Interval.Duration (Duration, DurationComponent(..))
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Prism (Prism', prism)
import Data.Map as Map
import Prelude (Unit, const, unit)

_Duration :: Lens' Duration (Map.Map DurationComponent Number)
_Duration = _Newtype

_Second :: Prism' DurationComponent Unit
_Second = prism (const Second) case _ of
  Second -> Right unit
  other -> Left other

_Minute :: Prism' DurationComponent Unit
_Minute = prism (const Minute) case _ of
  Minute -> Right unit
  other -> Left other

_Hour :: Prism' DurationComponent Unit
_Hour = prism (const Hour) case _ of
  Hour -> Right unit
  other -> Left other

_Day :: Prism' DurationComponent Unit
_Day = prism (const Day) case _ of
  Day -> Right unit
  other -> Left other

_Week :: Prism' DurationComponent Unit
_Week = prism (const Week) case _ of
  Week -> Right unit
  other -> Left other

_Month :: Prism' DurationComponent Unit
_Month = prism (const Month) case _ of
  Month -> Right unit
  other -> Left other

_Year :: Prism' DurationComponent Unit
_Year = prism (const Year) case _ of
  Year -> Right unit
  other -> Left other
