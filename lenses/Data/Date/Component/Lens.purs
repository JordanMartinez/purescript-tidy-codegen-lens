module Data.Date.Component.Lens where

import Data.Date.Component (Month(..), Weekday(..))
import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Prelude (Unit, const, unit)

_January :: Prism' Month Unit
_January = prism (const January) case _ of
  January -> Right unit
  other -> Left other

_February :: Prism' Month Unit
_February = prism (const February) case _ of
  February -> Right unit
  other -> Left other

_March :: Prism' Month Unit
_March = prism (const March) case _ of
  March -> Right unit
  other -> Left other

_April :: Prism' Month Unit
_April = prism (const April) case _ of
  April -> Right unit
  other -> Left other

_May :: Prism' Month Unit
_May = prism (const May) case _ of
  May -> Right unit
  other -> Left other

_June :: Prism' Month Unit
_June = prism (const June) case _ of
  June -> Right unit
  other -> Left other

_July :: Prism' Month Unit
_July = prism (const July) case _ of
  July -> Right unit
  other -> Left other

_August :: Prism' Month Unit
_August = prism (const August) case _ of
  August -> Right unit
  other -> Left other

_September :: Prism' Month Unit
_September = prism (const September) case _ of
  September -> Right unit
  other -> Left other

_October :: Prism' Month Unit
_October = prism (const October) case _ of
  October -> Right unit
  other -> Left other

_November :: Prism' Month Unit
_November = prism (const November) case _ of
  November -> Right unit
  other -> Left other

_December :: Prism' Month Unit
_December = prism (const December) case _ of
  December -> Right unit
  other -> Left other

_Monday :: Prism' Weekday Unit
_Monday = prism (const Monday) case _ of
  Monday -> Right unit
  other -> Left other

_Tuesday :: Prism' Weekday Unit
_Tuesday = prism (const Tuesday) case _ of
  Tuesday -> Right unit
  other -> Left other

_Wednesday :: Prism' Weekday Unit
_Wednesday = prism (const Wednesday) case _ of
  Wednesday -> Right unit
  other -> Left other

_Thursday :: Prism' Weekday Unit
_Thursday = prism (const Thursday) case _ of
  Thursday -> Right unit
  other -> Left other

_Friday :: Prism' Weekday Unit
_Friday = prism (const Friday) case _ of
  Friday -> Right unit
  other -> Left other

_Saturday :: Prism' Weekday Unit
_Saturday = prism (const Saturday) case _ of
  Saturday -> Right unit
  other -> Left other

_Sunday :: Prism' Weekday Unit
_Sunday = prism (const Sunday) case _ of
  Sunday -> Right unit
  other -> Left other
