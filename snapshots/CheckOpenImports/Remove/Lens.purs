module Snapshots.CheckOpenImports.Remove.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.Lens.Record (prop)
import Data.Tuple (Tuple(..))
import Prelude (Unit, const, unit)
import Snapshots.CheckOpenImports.Remove (Foo(..), One(..), Two(..))
import Type.Proxy (Proxy(..))

_One :: Lens' One Unit
_One = iso (const unit) (const One)

_Two :: Lens' Two Unit
_Two = iso (const unit) (const Two)

_Foo :: Lens' Foo (Tuple One Two)
_Foo = iso (\(Foo a b) -> Tuple a b) \(Tuple a b) -> Foo a b
