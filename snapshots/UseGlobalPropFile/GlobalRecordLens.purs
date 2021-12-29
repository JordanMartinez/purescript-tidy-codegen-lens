module Snapshots.UseGlobalPropFile.GlobalRecordLens where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

_propAliasOnly :: forall r a. Lens' { aliasOnly :: a | r } a
_propAliasOnly = prop (Proxy :: Proxy "aliasOnly")

_propArg1 :: forall r a. Lens' { arg1 :: a | r } a
_propArg1 = prop (Proxy :: Proxy "arg1")

_propArg10 :: forall r a. Lens' { arg10 :: a | r } a
_propArg10 = prop (Proxy :: Proxy "arg10")

_propArg11 :: forall r a. Lens' { arg11 :: a | r } a
_propArg11 = prop (Proxy :: Proxy "arg11")

_propArg12 :: forall r a. Lens' { arg12 :: a | r } a
_propArg12 = prop (Proxy :: Proxy "arg12")

_propArg13 :: forall r a. Lens' { arg13 :: a | r } a
_propArg13 = prop (Proxy :: Proxy "arg13")

_propArg2 :: forall r a. Lens' { arg2 :: a | r } a
_propArg2 = prop (Proxy :: Proxy "arg2")

_propArg3 :: forall r a. Lens' { arg3 :: a | r } a
_propArg3 = prop (Proxy :: Proxy "arg3")

_propArg4 :: forall r a. Lens' { arg4 :: a | r } a
_propArg4 = prop (Proxy :: Proxy "arg4")

_propArg5 :: forall r a. Lens' { arg5 :: a | r } a
_propArg5 = prop (Proxy :: Proxy "arg5")

_propArg6 :: forall r a. Lens' { arg6 :: a | r } a
_propArg6 = prop (Proxy :: Proxy "arg6")

_propArg7 :: forall r a. Lens' { arg7 :: a | r } a
_propArg7 = prop (Proxy :: Proxy "arg7")

_propArg8 :: forall r a. Lens' { arg8 :: a | r } a
_propArg8 = prop (Proxy :: Proxy "arg8")

_propArg9 :: forall r a. Lens' { arg9 :: a | r } a
_propArg9 = prop (Proxy :: Proxy "arg9")

_propBar :: forall r a. Lens' { bar :: a | r } a
_propBar = prop (Proxy :: Proxy "bar")

_propBaz :: forall r a. Lens' { baz :: a | r } a
_propBaz = prop (Proxy :: Proxy "baz")

_propBumblebee :: forall r a. Lens' { bumblebee :: a | r } a
_propBumblebee = prop (Proxy :: Proxy "bumblebee")

_propDefinedInSourceFile :: forall r a. Lens' { definedInSourceFile :: a | r } a
_propDefinedInSourceFile = prop (Proxy :: Proxy "definedInSourceFile")

_propFirst :: forall r a. Lens' { first :: a | r } a
_propFirst = prop (Proxy :: Proxy "first")

_propFoo :: forall r a. Lens' { foo :: a | r } a
_propFoo = prop (Proxy :: Proxy "foo")

_propNoAlias :: forall r a. Lens' { noAlias :: a | r } a
_propNoAlias = prop (Proxy :: Proxy "noAlias")

_propQuarts :: forall r a. Lens' { quarts :: a | r } a
_propQuarts = prop (Proxy :: Proxy "quarts")

_propSecond :: forall r a. Lens' { second :: a | r } a
_propSecond = prop (Proxy :: Proxy "second")

_propSharedAlias :: forall r a. Lens' { sharedAlias :: a | r } a
_propSharedAlias = prop (Proxy :: Proxy "sharedAlias")

_prop力 :: forall r a. Lens' { "力" :: a | r } a
_prop力 = prop (Proxy :: Proxy "力")
