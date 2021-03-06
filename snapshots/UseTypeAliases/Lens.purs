module Snapshots.UseTypeAliases.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso (Iso')
import Data.Lens.Record (prop)
import Prelude (identity)
import Snapshots.UseTypeAliases (TypeAlias_ArrayString, TypeAlias_Record, TypeAlias_String)
import Type.Proxy (Proxy(..))

_TypeAlias_Record :: Iso' TypeAlias_Record
  { foo :: Int
  , bar :: String
  , baz :: Boolean
  }
_TypeAlias_Record = identity

_TypeAlias_String :: Iso' TypeAlias_String String
_TypeAlias_String = identity

_TypeAlias_ArrayString :: Iso' TypeAlias_ArrayString (Array String)
_TypeAlias_ArrayString = identity

_propBar :: forall r a. Lens' { bar :: a | r } a
_propBar = prop (Proxy :: Proxy "bar")

_propBaz :: forall r a. Lens' { baz :: a | r } a
_propBaz = prop (Proxy :: Proxy "baz")

_propFoo :: forall r a. Lens' { foo :: a | r } a
_propFoo = prop (Proxy :: Proxy "foo")
