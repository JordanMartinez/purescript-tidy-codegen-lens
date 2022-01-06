module Data.DateTime.Lens where

import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.Time (Time)
import Data.Tuple (Tuple(..))

_DateTime :: Lens' DateTime (Tuple Date Time)
_DateTime = iso (\(DateTime a b) -> Tuple a b) \(Tuple a b) -> DateTime a b
