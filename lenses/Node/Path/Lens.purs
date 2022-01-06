module Node.Path.Lens where

import Data.Lens.Iso (Iso')
import Node.Path (FilePath)
import Prelude (identity)

_FilePath :: Iso' FilePath String
_FilePath = identity
