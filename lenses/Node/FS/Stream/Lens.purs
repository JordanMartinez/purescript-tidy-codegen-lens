module Node.FS.Stream.Lens where

import Prelude

import Data.Lens.Iso (Iso')
import Node.FS (FileFlags)
import Node.FS.Perms (Perms)
import Node.FS.Stream (ReadStreamOptions, WriteStreamOptions)
import Prelude (identity)

_WriteStreamOptions :: Iso' WriteStreamOptions
  { flags :: FileFlags
  , perms :: Perms
  }
_WriteStreamOptions = identity

_ReadStreamOptions :: Iso' ReadStreamOptions
  { flags :: FileFlags
  , perms :: Perms
  , autoClose :: Boolean
  }
_ReadStreamOptions = identity
