module Control.Monad.Writer.Lens where

import Control.Monad.Writer (Writer)
import Control.Monad.Writer.Trans (WriterT)
import Data.Identity (Identity)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_Writer :: forall w. Iso' (Writer w) (WriterT w Identity)
_Writer = identity
