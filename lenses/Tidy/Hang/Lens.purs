module Tidy.Hang.Lens where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Tidy.Doc (FormatDoc)
import Tidy.Hang (HangingDoc, HangingOp(..))

_HangingOp
  :: forall a. Lens' (HangingOp a) { arg1 :: Int, arg2 :: (FormatDoc a), arg3 :: (HangingDoc a) }
_HangingOp = iso (\(HangingOp arg1 arg2 arg3) -> { arg1: arg1, arg2: arg2, arg3: arg3 })
  \{ arg1, arg2, arg3 } -> HangingOp arg1 arg2 arg3
