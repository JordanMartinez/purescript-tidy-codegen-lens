module MkDir where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Node.Path (FilePath)

foreign import mkdirRec :: FilePath -> Effect Unit -> Effect Unit

mkdirRecAff :: FilePath -> Aff Unit
mkdirRecAff dir = makeAff \cb -> do
  mkdirRec dir (cb $ Right unit)
  pure nonCanceler
