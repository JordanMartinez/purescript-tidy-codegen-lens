module Snapshots.UseLabelPrefix.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Snapshots.UseLabelPrefix (Bar, Foo(..))
import Type.Proxy (Proxy(..))

_Foo :: Lens' Foo { a :: Int, b :: String }
_Foo = iso (\(Foo a) -> a) Foo

_Bar :: Lens' Bar { height :: Number }
_Bar = _Newtype

_propA :: forall r a. Lens' { a :: a | r } a
_propA = prop (Proxy :: Proxy "a")

_propB :: forall r a. Lens' { b :: a | r } a
_propB = prop (Proxy :: Proxy "b")

_propHeight :: forall r a. Lens' { height :: a | r } a
_propHeight = prop (Proxy :: Proxy "height")
