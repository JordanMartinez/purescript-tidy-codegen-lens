module Data.Time.Duration.Lens where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Time.Duration (Days, Hours, Milliseconds, Minutes, Seconds)

_Milliseconds :: Lens' Milliseconds Number
_Milliseconds = _Newtype

_Seconds :: Lens' Seconds Number
_Seconds = _Newtype

_Minutes :: Lens' Minutes Number
_Minutes = _Newtype

_Hours :: Lens' Hours Number
_Hours = _Newtype

_Days :: Lens' Days Number
_Days = _Newtype
