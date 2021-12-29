module Snapshots.UseGlobalPropFile.Module1.Lens where

import Data.Either (Either(..))
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Lens (lens)
import Data.Lens.Prism (Prism', prism)
import Data.Tuple (Tuple(..))
import Prelude (Unit, const, unit)
import Snapshots.Imports.ImportedExplicitTypesNoModuleAlias (MyAlias, MyData, MyNewtype)
import Snapshots.Imports.ImportedExplicitTypesWithModuleAlias as Shared
import Snapshots.Imports.ModuleAliasOnly as Q
import Snapshots.Imports.SharedModuleAlias as Shared

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
