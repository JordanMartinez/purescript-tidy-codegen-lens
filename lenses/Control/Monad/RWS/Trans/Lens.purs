module Control.Monad.RWS.Trans.Lens where

import Control.Monad.RWS.Trans (RWST, RWSResult(..))
import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.Lens.Iso.Newtype (_Newtype)

_RWSResult
  :: forall state result writer
   . Lens' (RWSResult state result writer) { arg1 :: state, arg2 :: result, arg3 :: writer }
_RWSResult = iso (\(RWSResult arg1 arg2 arg3) -> { arg1: arg1, arg2: arg2, arg3: arg3 })
  \{ arg1, arg2, arg3 } -> RWSResult arg1 arg2 arg3

_RWST :: forall r w s m a. Lens' (RWST r w s m a) (r -> s -> m (RWSResult s a w))
_RWST = _Newtype
