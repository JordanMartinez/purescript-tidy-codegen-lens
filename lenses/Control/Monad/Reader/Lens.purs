module Control.Monad.Reader.Lens where

import Control.Monad.Reader (Reader)
import Control.Monad.Reader.Trans (ReaderT)
import Data.Identity (Identity)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_Reader :: forall r. Iso' (Reader r) (ReaderT r Identity)
_Reader = identity
