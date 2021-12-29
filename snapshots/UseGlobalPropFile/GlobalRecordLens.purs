module Snapshots.UseGlobalPropFile.GlobalRecordLens where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

_propArg1 :: forall r a. Lens' { arg1 :: a | r } a
_propArg1 = prop (Proxy :: Proxy "arg1")

_propArg2 :: forall r a. Lens' { arg2 :: a | r } a
_propArg2 = prop (Proxy :: Proxy "arg2")

_propArg3 :: forall r a. Lens' { arg3 :: a | r } a
_propArg3 = prop (Proxy :: Proxy "arg3")

_propBar :: forall r a. Lens' { bar :: a | r } a
_propBar = prop (Proxy :: Proxy "bar")

_propBaz :: forall r a. Lens' { baz :: a | r } a
_propBaz = prop (Proxy :: Proxy "baz")

_propBumblebee :: forall r a. Lens' { bumblebee :: a | r } a
_propBumblebee = prop (Proxy :: Proxy "bumblebee")

_propFirst :: forall r a. Lens' { first :: a | r } a
_propFirst = prop (Proxy :: Proxy "first")

_propFoo :: forall r a. Lens' { foo :: a | r } a
_propFoo = prop (Proxy :: Proxy "foo")

_propQuarts :: forall r a. Lens' { quarts :: a | r } a
_propQuarts = prop (Proxy :: Proxy "quarts")

_propSecond :: forall r a. Lens' { second :: a | r } a
_propSecond = prop (Proxy :: Proxy "second")

_prop力 :: forall r a. Lens' { "力" :: a | r } a
_prop力 = prop (Proxy :: Proxy "力")
