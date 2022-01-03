module Snapshots.CheckOpenImports.RemoveHiding where

-- This open import should be re-imported
-- but without the 'NameClash' member hidden
-- since it doesn't clash with any other types.
-- However, this may significantly slow down the implementation,
-- so I'm not handling this case yet because of how rare it likely is.
import Snapshots.Imports.OpenImportNameClashes hiding (NameClash)

data One = One
data Two = Two

data Foo = Foo One Two NameIsFine
