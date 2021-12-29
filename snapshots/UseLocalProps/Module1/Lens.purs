module Snapshots.UseLocalProps.Module1.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Lens (lens)
import Data.Lens.Record (prop)
import Data.Tuple (Tuple(..))
import Prelude (Unit, const, unit)
import Type.Proxy (Proxy(..))

_Data_NoTyVars_Args0 :: Lens' Data_NoTyVars_Args0 Unit
_Data_NoTyVars_Args0 = lens (const unit) (const Data_NoTyVars_Args0)

_Data_NoTyVars_Args1 :: Lens' Data_NoTyVars_Args1 Int
_Data_NoTyVars_Args1 = lens (\(Data_NoTyVars_Args1 a) -> a) Data_NoTyVars_Args1

_Data_NoTyVars_Args2 :: Lens' Data_NoTyVars_Args2 (Tuple Int String)
_Data_NoTyVars_Args2 = lens (\(Data_NoTyVars_Args2 a b) -> Tuple a b) \(Tuple a b) ->
  Data_NoTyVars_Args2 a b

_Data_NoTyVars_Args3 :: Lens' Data_NoTyVars_Args3 { arg1 :: Int, arg2 :: String, arg3 :: Boolean }
_Data_NoTyVars_Args3 = lens
  (\(Data_NoTyVars_Args3 arg1 arg2 arg3) -> { arg1: arg1, arg2: arg2, arg3: arg3 })
  \{ arg1, arg2, arg3 } -> Data_NoTyVars_Args3 arg1 arg2 arg3

_Data_TyVars_Args0 :: forall a b c d. Lens' (Data_TyVars_Args0 a b c d) Unit
_Data_TyVars_Args0 = lens (const unit) (const Data_TyVars_Args0)

_Data_TyVars_Args1 :: forall a b c d. Lens' (Data_TyVars_Args1 a b c d) a
_Data_TyVars_Args1 = lens (\(Data_TyVars_Args1 a) -> a) Data_TyVars_Args1

_Data_TyVars_Args2 :: forall a b c d. Lens' (Data_TyVars_Args2 a b c d) (Tuple a b)
_Data_TyVars_Args2 = lens (\(Data_TyVars_Args2 a b) -> Tuple a b) \(Tuple a b) -> Data_TyVars_Args2
  a
  b

_Data_TyVars_Args3
  :: forall a b c d. Lens' (Data_TyVars_Args3 a b c d) { arg1 :: a, arg2 :: b, arg3 :: c }
_Data_TyVars_Args3 = lens
  (\(Data_TyVars_Args3 arg1 arg2 arg3) -> { arg1: arg1, arg2: arg2, arg3: arg3 })
  \{ arg1, arg2, arg3 } -> Data_TyVars_Args3 arg1 arg2 arg3

_NewtypedRecord :: Lens' NewtypedRecord
  { first :: String
  , second :: String
  }
_NewtypedRecord = _Newtype

_NewtypedType :: Lens' NewtypedType Int
_NewtypedType = _Newtype

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
