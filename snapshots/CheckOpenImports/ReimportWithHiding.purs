module Snapshots.CheckOpenImports.ReimportWitHiding where

-- This open import should be re-imported
-- since the 'OpenImportNameClashes.Ordering'
-- type's name clashes with the `Prelude.Ordering` type's name
import Snapshots.Imports.OpenImportNameClashes hiding (Ordering(..))

import Prelude

data One = One
data Two = Two

data Foo = Foo One Two Ordering
