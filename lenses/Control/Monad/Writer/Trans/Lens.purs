module Control.Monad.Writer.Trans.Lens where

import Control.Monad.Writer.Trans (WriterT)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Tuple (Tuple)

_WriterT :: forall w m a. Lens' (WriterT w m a) (m (Tuple a w))
_WriterT = _Newtype
