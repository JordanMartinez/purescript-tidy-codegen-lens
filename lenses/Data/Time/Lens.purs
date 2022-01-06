module Data.Time.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.Time (Time(..))
import Data.Time.Component (Hour, Millisecond, Minute, Second)

_Time :: Lens' Time { arg1 :: Hour, arg2 :: Minute, arg3 :: Second, arg4 :: Millisecond }
_Time = iso (\(Time arg1 arg2 arg3 arg4) -> { arg1: arg1, arg2: arg2, arg3: arg3, arg4: arg4 })
  \{ arg1, arg2, arg3, arg4 } -> Time arg1 arg2 arg3 arg4
