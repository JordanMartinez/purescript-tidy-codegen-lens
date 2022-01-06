module Node.FS.Async.Lens where

import Prelude

import Data.Either (Either)
import Data.Lens.Iso (Iso')
import Effect (Effect)
import Effect.Exception (Error)
import Node.FS.Async (Callback)
import Prelude (identity)

_Callback :: forall a. Iso' (Callback a) (Either Error a -> Effect Unit)
_Callback = identity
