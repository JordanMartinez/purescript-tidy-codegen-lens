module Data.Interval.Lens where

import Prelude

import Data.Either (Either(..))
import Data.Interval (Interval(..), RecurringInterval(..))
import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.Lens.Prism (Prism', prism)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

_RecurringInterval :: forall d a. Lens' (RecurringInterval d a) (Tuple (Maybe Int) (Interval d a))
_RecurringInterval = iso (\(RecurringInterval a b) -> Tuple a b) \(Tuple a b) -> RecurringInterval
  a
  b

_StartEnd :: forall d a. Prism' (Interval d a) (Tuple a a)
_StartEnd = prism (\(Tuple a b) -> StartEnd a b) case _ of
  StartEnd a b -> Right (Tuple a b)
  other -> Left other

_DurationEnd :: forall d a. Prism' (Interval d a) (Tuple d a)
_DurationEnd = prism (\(Tuple a b) -> DurationEnd a b) case _ of
  DurationEnd a b -> Right (Tuple a b)
  other -> Left other

_StartDuration :: forall d a. Prism' (Interval d a) (Tuple a d)
_StartDuration = prism (\(Tuple a b) -> StartDuration a b) case _ of
  StartDuration a b -> Right (Tuple a b)
  other -> Left other

_DurationOnly :: forall d a. Prism' (Interval d a) d
_DurationOnly = prism DurationOnly case _ of
  DurationOnly a -> Right a
  other -> Left other
