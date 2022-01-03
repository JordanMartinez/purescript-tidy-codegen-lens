module Snapshots.CheckOpenImports.Remove where

-- This open import should be removed
-- since all referenced types are findable.
import Prelude

data One = One
data Two = Two

data Foo = Foo One Two
