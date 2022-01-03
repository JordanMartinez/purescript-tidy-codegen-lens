module Snapshots.CheckOpenImports.ReimportWitHiding.Lens where

-- import Prelude

-- import Snapshots.Imports.OpenImportNameClashes hiding (Ordering)

-- import Data.Lens (Lens')
-- import Data.Lens.Iso (iso)
-- import Data.Lens.Record (prop)
-- import Prelude (Unit, const, unit)
-- import Snapshots.CheckOpenImports.ReimportWitHiding (Foo(..), One(..), Two(..))
-- import Type.Proxy (Proxy(..))

-- _One :: Lens' One Unit
-- _One = iso (const unit) (const One)

-- _Two :: Lens' Two Unit
-- _Two = iso (const unit) (const Two)

-- _Foo :: Lens' Foo { arg1 :: One, arg2 :: Two, arg3 :: Ordering }
-- _Foo = iso (\(Foo arg1 arg2 arg3) -> { arg1: arg1, arg2: arg2, arg3: arg3 })
--   \{ arg1, arg2, arg3 } -> Foo arg1 arg2 arg3

-- _propArg1 :: forall r a. Lens' { arg1 :: a | r } a
-- _propArg1 = prop (Proxy :: Proxy "arg1")

-- _propArg2 :: forall r a. Lens' { arg2 :: a | r } a
-- _propArg2 = prop (Proxy :: Proxy "arg2")

-- _propArg3 :: forall r a. Lens' { arg3 :: a | r } a
-- _propArg3 = prop (Proxy :: Proxy "arg3")
