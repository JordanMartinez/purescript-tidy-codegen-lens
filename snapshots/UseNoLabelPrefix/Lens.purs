module Snapshots.UseNoLabelPrefix.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Snapshots.UseNoLabelPrefix (Bar, Foo(..))
import Type.Proxy (Proxy(..))

_Foo :: Lens' Foo { a :: Int, b :: String }
_Foo = iso (\(Foo a) -> a) Foo

_Bar :: Lens' Bar { height :: Number }
_Bar = _Newtype

_a :: forall r a. Lens' { a :: a | r } a
_a = prop (Proxy :: Proxy "a")

_b :: forall r a. Lens' { b :: a | r } a
_b = prop (Proxy :: Proxy "b")

_height :: forall r a. Lens' { height :: a | r } a
_height = prop (Proxy :: Proxy "height")
