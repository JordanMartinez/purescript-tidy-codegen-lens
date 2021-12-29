module Snapshots.PrimaryExample.Lens where

import Prelude

import Data.Either (Either(..))
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Lens (lens)
import Data.Lens.Prism (Prism', prism)
import Data.Lens.Record (prop)
import Data.Tuple (Tuple(..))
import Prelude (Unit, const, unit)
import Snapshots.Imports.ImportedExplicitTypesNoModuleAlias (MyAlias, MyData, MyNewtype)
import Snapshots.Imports.ImportedExplicitTypesWithModuleAlias as Shared
import Snapshots.Imports.ModuleAliasOnly as Q
import Snapshots.Imports.SharedModuleAlias as Shared
import Snapshots.PrimaryExample (AliasDefinedInSourceFile, FfiTypeDefinedInSourceFile, DataDefinedInSourceFile(..), Data_NoTyVars_Args0(..), Data_NoTyVars_Args1(..), Data_NoTyVars_Args2(..), Data_NoTyVars_Args3(..), Data_NoTyVars_Sum_Args0(..), Data_NoTyVars_Sum_Args1(..), Data_NoTyVars_Sum_Args2(..), Data_NoTyVars_Sum_Args3(..), Data_Product_ImportedTypesAreReimported(..), Data_Sum_ImportedTypesAreReimported(..), Data_TyVars_Args0(..), Data_TyVars_Args1(..), Data_TyVars_Args2(..), Data_TyVars_Args3(..), Data_TyVars_Sum_Args0(..), Data_TyVars_Sum_Args1(..), Data_TyVars_Sum_Args2(..), Data_TyVars_Sum_Args3(..), EnsureSharedImportIsUsed(..), NewtypeDefinedInSourceFile(..), NewtypedRecord(..), NewtypedType(..), Record_ImportedTypesAreReimported(..))
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

_Data_NoTyVars_Sum_Args0 :: Prism' Data_NoTyVars_Sum_Args0 Unit
_Data_NoTyVars_Sum_Args0 = prism (const Data_NoTyVars_Sum_Args0) case _ of
  Data_NoTyVars_Sum_Args0 -> Right unit
  other -> Left other

_Data_NoTyVars_Sum_Args0_Ignored :: Prism' Data_NoTyVars_Sum_Args0 Unit
_Data_NoTyVars_Sum_Args0_Ignored = prism (const Data_NoTyVars_Sum_Args0_Ignored) case _ of
  Data_NoTyVars_Sum_Args0_Ignored -> Right unit
  other -> Left other

_Data_NoTyVars_Sum_Args1 :: Prism' Data_NoTyVars_Sum_Args1 Int
_Data_NoTyVars_Sum_Args1 = prism Data_NoTyVars_Sum_Args1 case _ of
  Data_NoTyVars_Sum_Args1 a -> Right a
  other -> Left other

_Data_NoTyVars_Sum_Args1_Ignored :: Prism' Data_NoTyVars_Sum_Args1 Unit
_Data_NoTyVars_Sum_Args1_Ignored = prism (const Data_NoTyVars_Sum_Args1_Ignored) case _ of
  Data_NoTyVars_Sum_Args1_Ignored -> Right unit
  other -> Left other

_Data_NoTyVars_Sum_Args2 :: Prism' Data_NoTyVars_Sum_Args2 (Tuple Int Int)
_Data_NoTyVars_Sum_Args2 = prism (\(Tuple a b) -> Data_NoTyVars_Sum_Args2 a b) case _ of
  Data_NoTyVars_Sum_Args2 a b -> Right (Tuple a b)
  other -> Left other

_Data_NoTyVars_Sum_Args2_Ignored :: Prism' Data_NoTyVars_Sum_Args2 Unit
_Data_NoTyVars_Sum_Args2_Ignored = prism (const Data_NoTyVars_Sum_Args2_Ignored) case _ of
  Data_NoTyVars_Sum_Args2_Ignored -> Right unit
  other -> Left other

_Data_NoTyVars_Sum_Args3 :: Prism' Data_NoTyVars_Sum_Args3
  { arg1 :: Int, arg2 :: Int, arg3 :: Int }
_Data_NoTyVars_Sum_Args3 = prism (\{ arg1, arg2, arg3 } -> Data_NoTyVars_Sum_Args3 arg1 arg2 arg3)
  case _ of
    Data_NoTyVars_Sum_Args3 arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
    other -> Left other

_Data_NoTyVars_Sum_Args3_Ignored :: Prism' Data_NoTyVars_Sum_Args3 Unit
_Data_NoTyVars_Sum_Args3_Ignored = prism (const Data_NoTyVars_Sum_Args3_Ignored) case _ of
  Data_NoTyVars_Sum_Args3_Ignored -> Right unit
  other -> Left other

_Data_TyVars_Sum_Args0 :: forall a b c d. Prism' (Data_TyVars_Sum_Args0 a b c d) Unit
_Data_TyVars_Sum_Args0 = prism (const Data_TyVars_Sum_Args0) case _ of
  Data_TyVars_Sum_Args0 -> Right unit
  other -> Left other

_Data_TyVars_Sum_Args0_Ignored :: forall a b c d. Prism' (Data_TyVars_Sum_Args0 a b c d) Unit
_Data_TyVars_Sum_Args0_Ignored = prism (const Data_TyVars_Sum_Args0_Ignored) case _ of
  Data_TyVars_Sum_Args0_Ignored -> Right unit
  other -> Left other

_Data_TyVars_Sum_Args1 :: forall a b c d. Prism' (Data_TyVars_Sum_Args1 a b c d) a
_Data_TyVars_Sum_Args1 = prism Data_TyVars_Sum_Args1 case _ of
  Data_TyVars_Sum_Args1 a -> Right a
  other -> Left other

_Data_TyVars_Sum_Args1_Ignored :: forall a b c d. Prism' (Data_TyVars_Sum_Args1 a b c d) Unit
_Data_TyVars_Sum_Args1_Ignored = prism (const Data_TyVars_Sum_Args1_Ignored) case _ of
  Data_TyVars_Sum_Args1_Ignored -> Right unit
  other -> Left other

_Data_TyVars_Sum_Args2 :: forall a b c d. Prism' (Data_TyVars_Sum_Args2 a b c d) (Tuple a b)
_Data_TyVars_Sum_Args2 = prism (\(Tuple a b) -> Data_TyVars_Sum_Args2 a b) case _ of
  Data_TyVars_Sum_Args2 a b -> Right (Tuple a b)
  other -> Left other

_Data_TyVars_Sum_Args2_Ignored :: forall a b c d. Prism' (Data_TyVars_Sum_Args2 a b c d) Unit
_Data_TyVars_Sum_Args2_Ignored = prism (const Data_TyVars_Sum_Args2_Ignored) case _ of
  Data_TyVars_Sum_Args2_Ignored -> Right unit
  other -> Left other

_Data_TyVars_Sum_Args3
  :: forall a b c d. Prism' (Data_TyVars_Sum_Args3 a b c d) { arg1 :: a, arg2 :: b, arg3 :: c }
_Data_TyVars_Sum_Args3 = prism (\{ arg1, arg2, arg3 } -> Data_TyVars_Sum_Args3 arg1 arg2 arg3)
  case _ of
    Data_TyVars_Sum_Args3 arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
    other -> Left other

_Data_TyVars_Sum_Args3_Ignored :: forall a b c d. Prism' (Data_TyVars_Sum_Args3 a b c d) Unit
_Data_TyVars_Sum_Args3_Ignored = prism (const Data_TyVars_Sum_Args3_Ignored) case _ of
  Data_TyVars_Sum_Args3_Ignored -> Right unit
  other -> Left other

_NewtypedRecord :: Lens' NewtypedRecord
  { first :: String
  , second :: String
  }
_NewtypedRecord = _Newtype

_NewtypedType :: Lens' NewtypedType Int
_NewtypedType = _Newtype

_DataDefinedInSourceFile :: Lens' DataDefinedInSourceFile Unit
_DataDefinedInSourceFile = lens (const unit) (const DataDefinedInSourceFile)

_NewtypeDefinedInSourceFile :: Lens' NewtypeDefinedInSourceFile Int
_NewtypeDefinedInSourceFile = _Newtype

_Data_Product_ImportedTypesAreReimported :: Lens' Data_Product_ImportedTypesAreReimported
  { arg1 ::
      (MyData Int Int Int)
  , arg2 :: (MyAlias String)
  , arg3 :: (MyNewtype Int Int Int)
  , arg4 ::
      (Q.MyData Int Int Int)
  , arg5 :: (Q.MyAlias String)
  , arg6 :: (Q.MyNewtype Int Int Int)
  , arg7 ::
      (Shared.MyData Int Int Int)
  , arg8 :: (Shared.MyAlias String)
  , arg9 :: (Shared.MyNewtype Int Int Int)
  , arg10 ::
      DataDefinedInSourceFile
  , arg11 :: NewtypeDefinedInSourceFile
  , arg12 :: AliasDefinedInSourceFile
  , arg13 :: FfiTypeDefinedInSourceFile
  }
_Data_Product_ImportedTypesAreReimported = lens
  ( \( Data_Product_ImportedTypesAreReimported arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10
         arg11
         arg12
         arg13
     ) ->
      { arg1: arg1
      , arg2: arg2
      , arg3: arg3
      , arg4: arg4
      , arg5: arg5
      , arg6: arg6
      , arg7: arg7
      , arg8: arg8
      , arg9: arg9
      , arg10: arg10
      , arg11: arg11
      , arg12: arg12
      , arg13: arg13
      }
  )
  \{ arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13 } ->
    Data_Product_ImportedTypesAreReimported arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11
      arg12
      arg13

_Data_Sum_ImportedTypesAreReimported :: Prism' Data_Sum_ImportedTypesAreReimported
  { arg1 ::
      (MyData Int Int Int)
  , arg2 :: (MyAlias String)
  , arg3 :: (MyNewtype Int Int Int)
  , arg4 ::
      (Q.MyData Int Int Int)
  , arg5 :: (Q.MyAlias String)
  , arg6 :: (Q.MyNewtype Int Int Int)
  , arg7 ::
      (Shared.MyData Int Int Int)
  , arg8 :: (Shared.MyAlias String)
  , arg9 :: (Shared.MyNewtype Int Int Int)
  , arg10 ::
      DataDefinedInSourceFile
  , arg11 :: NewtypeDefinedInSourceFile
  , arg12 :: AliasDefinedInSourceFile
  , arg13 :: FfiTypeDefinedInSourceFile
  }
_Data_Sum_ImportedTypesAreReimported = prism
  ( \{ arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13 } ->
      Data_Sum_ImportedTypesAreReimported arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11
        arg12
        arg13
  )
  case _ of
    Data_Sum_ImportedTypesAreReimported arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11
      arg12
      arg13 -> Right
      { arg1: arg1
      , arg2: arg2
      , arg3: arg3
      , arg4: arg4
      , arg5: arg5
      , arg6: arg6
      , arg7: arg7
      , arg8: arg8
      , arg9: arg9
      , arg10: arg10
      , arg11: arg11
      , arg12: arg12
      , arg13: arg13
      }
    other -> Left other

_Data_Sum_ImportedTypesAreReimported_IgnoredCase :: Prism' Data_Sum_ImportedTypesAreReimported Unit
_Data_Sum_ImportedTypesAreReimported_IgnoredCase = prism
  (const Data_Sum_ImportedTypesAreReimported_IgnoredCase)
  case _ of
    Data_Sum_ImportedTypesAreReimported_IgnoredCase -> Right unit
    other -> Left other

_Record_ImportedTypesAreReimported :: Lens' Record_ImportedTypesAreReimported
  { noAlias ::
      { myData :: MyData Int Int Int
      , myAlias :: MyAlias String
      , myNewtype :: MyNewtype Int Int Int
      }
  , aliasOnly ::
      { myData :: Q.MyData Int Int Int
      , myAlias :: Q.MyAlias String
      , myNewtype :: Q.MyNewtype Int Int Int
      }
  , sharedAlias ::
      { myData :: Shared.MyData Int Int Int
      , myAlias :: Shared.MyAlias String
      , myNewtype :: Shared.MyNewtype Int Int Int
      }
  , definedInSourceFile ::
      { myData :: DataDefinedInSourceFile
      , myAlias :: AliasDefinedInSourceFile
      , myNewtype :: NewtypeDefinedInSourceFile
      , myFfi :: FfiTypeDefinedInSourceFile
      }
  }
_Record_ImportedTypesAreReimported = _Newtype

_EnsureSharedImportIsUsed :: Lens' EnsureSharedImportIsUsed
  { arg1 ::
      (Shared.SharedMyData Int Int Int)
  , arg2 :: (Shared.SharedMyAlias String)
  , arg3 :: (Shared.SharedMyNewtype Int Int Int)
  }
_EnsureSharedImportIsUsed = lens
  ( \(EnsureSharedImportIsUsed arg1 arg2 arg3) -> { arg1: arg1, arg2: arg2, arg3: arg3 }
  )
  \{ arg1, arg2, arg3 } ->
    EnsureSharedImportIsUsed arg1 arg2 arg3

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

_propDefinedInSourceFile :: forall r a. Lens' { definedInSourceFile :: a | r } a
_propDefinedInSourceFile = prop (Proxy :: Proxy "definedInSourceFile")

_propFirst :: forall r a. Lens' { first :: a | r } a
_propFirst = prop (Proxy :: Proxy "first")

_propFoo :: forall r a. Lens' { foo :: a | r } a
_propFoo = prop (Proxy :: Proxy "foo")

_propNoAlias :: forall r a. Lens' { noAlias :: a | r } a
_propNoAlias = prop (Proxy :: Proxy "noAlias")

_propSecond :: forall r a. Lens' { second :: a | r } a
_propSecond = prop (Proxy :: Proxy "second")

_propSharedAlias :: forall r a. Lens' { sharedAlias :: a | r } a
_propSharedAlias = prop (Proxy :: Proxy "sharedAlias")
