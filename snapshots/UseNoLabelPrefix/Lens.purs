module Snapshots.UseNoLabelPrefix.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.Lens.Record (prop)
import Snapshots.UseNoLabelPrefix (Foo(..))
import Type.Proxy (Proxy(..))

_Foo :: Lens' Foo { a :: Int, b :: String }
_Foo = iso (\(Foo a) -> a) Foo

_a :: forall r a. Lens' { a :: a | r } a
_a = prop (Proxy :: Proxy "a")

_b :: forall r a. Lens' { b :: a | r } a
_b = prop (Proxy :: Proxy "b")

_bar :: forall r a. Lens' { bar :: a | r } a
_bar = prop (Proxy :: Proxy "bar")

_baz :: forall r a. Lens' { baz :: a | r } a
_baz = prop (Proxy :: Proxy "baz")

_foo :: forall r a. Lens' { foo :: a | r } a
_foo = prop (Proxy :: Proxy "foo")
