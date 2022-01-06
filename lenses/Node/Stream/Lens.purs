module Node.Stream.Lens where

import Prelude

import Data.Lens.Iso (Iso')
import Node.Stream (Duplex, Readable, Stream, Writable)
import Prelude (identity)

_Readable :: forall r. Iso' (Readable r) (Stream (read :: Read | r))
_Readable = identity

_Writable :: forall r. Iso' (Writable r) (Stream (write :: Write | r))
_Writable = identity

_Duplex :: Iso' Duplex (Stream (read :: Read, write :: Write))
_Duplex = identity
