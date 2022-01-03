module Snapshots.CheckOpenImports.Reimport where

-- This open import should be re-imported
-- since the 'Ordering' type wasn't found.
import Snapshots.Imports.OpenImportNameClashes

data One = One
data Two = Two

data Foo = Foo One Two Ordering
