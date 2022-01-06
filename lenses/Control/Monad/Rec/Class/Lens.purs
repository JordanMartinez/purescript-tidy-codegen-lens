module Control.Monad.Rec.Class.Lens where

import Control.Monad.Rec.Class (Step(..))
import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)

_Loop :: forall a b. Prism' (Step a b) a
_Loop = prism Loop case _ of
  Loop a -> Right a
  other -> Left other

_Done :: forall a b. Prism' (Step a b) b
_Done = prism Done case _ of
  Done a -> Right a
  other -> Left other
