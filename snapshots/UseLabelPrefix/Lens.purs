module Snapshots.UseLabelPrefix.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.Lens.Record (prop)
import Snapshots.UseLabelPrefix (Foo(..))
import Type.Proxy (Proxy(..))

_Foo :: Lens' Foo { a :: Int, b :: String }
_Foo = iso (\(Foo a) -> a) Foo

_propA :: forall r a. Lens' { a :: a | r } a
_propA = prop (Proxy :: Proxy "a")

_propB :: forall r a. Lens' { b :: a | r } a
_propB = prop (Proxy :: Proxy "b")

_propBar :: forall r a. Lens' { bar :: a | r } a
_propBar = prop (Proxy :: Proxy "bar")

_propBaz :: forall r a. Lens' { baz :: a | r } a
_propBaz = prop (Proxy :: Proxy "baz")

_propFoo :: forall r a. Lens' { foo :: a | r } a
_propFoo = prop (Proxy :: Proxy "foo")
