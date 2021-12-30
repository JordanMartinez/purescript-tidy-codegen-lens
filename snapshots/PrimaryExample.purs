module Snapshots.PrimaryExample
  ( AliasDefinedInSourceFile
  , DataDefinedInSourceFile(..)
  , Data_NoTyVars_Args0(..)
  , Data_NoTyVars_Args1(..)
  , Data_NoTyVars_Args2(..)
  , Data_NoTyVars_Args3(..)
  , Data_NoTyVars_Sum_Args0(..)
  , Data_NoTyVars_Sum_Args1(..)
  , Data_NoTyVars_Sum_Args2(..)
  , Data_NoTyVars_Sum_Args3(..)
  , Data_Product_ImportedTypesAreReimported(..)
  , Data_Sum_ImportedTypesAreReimported(..)
  , Data_TyVars_Args0(..)
  , Data_TyVars_Args1(..)
  , Data_TyVars_Args2(..)
  , Data_TyVars_Args3(..)
  , Data_TyVars_Sum_Args0(..)
  , Data_TyVars_Sum_Args1(..)
  , Data_TyVars_Sum_Args2(..)
  , Data_TyVars_Sum_Args3(..)
  , Data_TypeExported_CtorExported_Product_All(..)
  , Data_TypeExported_CtorExported_Sum_All(..)
  -- , Data_TypeNotExported(..)
  , Data_TypeOnlyExported
  , Data_Wrapping_Record(..)
  , EnsureSharedImportIsUsed(..)
  , FfiTypeDefinedInSourceFile
  , NewtypeDefinedInSourceFile(..)
  , Newtype_TypeExported_CtorExported_All(..)
  , Newtype_TypeExported_CtorExported_ByName(Newtype_TypeExported_CtorExported_ByName)
  -- , Newtype_TypeNotExported(..)
  , Newtype_TypeOnlyExported
  , NewtypedRecord(..)
  , NewtypedType(..)
  , Record_ImportedTypesAreReimported(..)
  , TypeAlias_Record
  , TypeAlias_Type
  , TypeAlias_TypeExported
  -- , TypeAlias_TypeNotExported
  , usePrelude
  )
  where

import Prelude

import Prim.Row hiding (class Cons)
import Prim hiding (Char)
import Prim.Boolean hiding (True) as PB

import Snapshots.Imports.ImportedExplicitTypesNoModuleAlias (MyAlias, MyData, MyNewtype)
import Snapshots.Imports.ImportedExplicitTypesWithModuleAlias (MyData, MyNewtype, MyAlias) as Shared
import Snapshots.Imports.ModuleAliasOnly as Q
import Snapshots.Imports.SharedModuleAlias as Shared

data Data_NoTyVars_Args0 = Data_NoTyVars_Args0

data Data_NoTyVars_Args1 = Data_NoTyVars_Args1 Int

data Data_NoTyVars_Args2 = Data_NoTyVars_Args2 Int String

data Data_NoTyVars_Args3 = Data_NoTyVars_Args3 Int String Boolean

data Data_TyVars_Args0 :: Type -> Type -> Type -> Type -> Type
data Data_TyVars_Args0 a b c d = Data_TyVars_Args0

data Data_TyVars_Args1 :: Type -> Type -> Type -> Type -> Type
data Data_TyVars_Args1 a b c d = Data_TyVars_Args1 a

data Data_TyVars_Args2 :: Type -> Type -> Type -> Type -> Type
data Data_TyVars_Args2 a b c d = Data_TyVars_Args2 a b

data Data_TyVars_Args3 :: Type -> Type -> Type -> Type -> Type
data Data_TyVars_Args3 a b c d = Data_TyVars_Args3 a b c

data Data_NoTyVars_Sum_Args0
  = Data_NoTyVars_Sum_Args0
  | Data_NoTyVars_Sum_Args0_Ignored

data Data_NoTyVars_Sum_Args1
  = Data_NoTyVars_Sum_Args1 Int
  | Data_NoTyVars_Sum_Args1_Ignored

data Data_NoTyVars_Sum_Args2
  = Data_NoTyVars_Sum_Args2 Int Int
  | Data_NoTyVars_Sum_Args2_Ignored

data Data_NoTyVars_Sum_Args3
  = Data_NoTyVars_Sum_Args3 Int Int Int
  | Data_NoTyVars_Sum_Args3_Ignored

data Data_TyVars_Sum_Args0 :: Type -> Type -> Type -> Type -> Type
data Data_TyVars_Sum_Args0 a b c d
  = Data_TyVars_Sum_Args0
  | Data_TyVars_Sum_Args0_Ignored

data Data_TyVars_Sum_Args1 :: Type -> Type -> Type -> Type -> Type
data Data_TyVars_Sum_Args1 a b c d
  = Data_TyVars_Sum_Args1 a
  | Data_TyVars_Sum_Args1_Ignored

data Data_TyVars_Sum_Args2 :: Type -> Type -> Type -> Type -> Type
data Data_TyVars_Sum_Args2 a b c d
  = Data_TyVars_Sum_Args2 a b
  | Data_TyVars_Sum_Args2_Ignored

data Data_TyVars_Sum_Args3 :: Type -> Type -> Type -> Type -> Type
data Data_TyVars_Sum_Args3 a b c d
  = Data_TyVars_Sum_Args3 a b c
  | Data_TyVars_Sum_Args3_Ignored

data Data_Wrapping_Record a b c
  = Data_Wrapping_Record
      { recordFieldReferencedInExportedData :: a
      }

type TypeAlias_Record =
  { recordFieldReferencedInExportedTypeAlias :: Int
  , bar :: String
  , baz :: Boolean
  }

type TypeAlias_Type = String

newtype NewtypedRecord = NewtypedRecord
  { first :: String
  , second :: String
  }

newtype NewtypedType = NewtypedType Int

data DataDefinedInSourceFile = DataDefinedInSourceFile
newtype NewtypeDefinedInSourceFile = NewtypeDefinedInSourceFile Int
type AliasDefinedInSourceFile = Int
foreign import data FfiTypeDefinedInSourceFile :: Type

data Data_Product_ImportedTypesAreReimported
  = Data_Product_ImportedTypesAreReimported
      (MyData Int Int Int) (MyAlias String) (MyNewtype Int Int Int)
      (Q.MyData Int Int Int) (Q.MyAlias String) (Q.MyNewtype Int Int Int)
      (Shared.MyData Int Int Int) (Shared.MyAlias String) (Shared.MyNewtype Int Int Int)
      DataDefinedInSourceFile NewtypeDefinedInSourceFile AliasDefinedInSourceFile FfiTypeDefinedInSourceFile

data Data_Sum_ImportedTypesAreReimported
  = Data_Sum_ImportedTypesAreReimported
      (MyData Int Int Int) (MyAlias String) (MyNewtype Int Int Int)
      (Q.MyData Int Int Int) (Q.MyAlias String) (Q.MyNewtype Int Int Int)
      (Shared.MyData Int Int Int) (Shared.MyAlias String) (Shared.MyNewtype Int Int Int)
      DataDefinedInSourceFile NewtypeDefinedInSourceFile AliasDefinedInSourceFile FfiTypeDefinedInSourceFile
  | Data_Sum_ImportedTypesAreReimported_IgnoredCase

newtype Record_ImportedTypesAreReimported = Record_ImportedTypesAreReimported
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

data Data_TypeNotExported = Data_TypeNotExported
data Data_TypeOnlyExported = Data_TypeOnlyExported
data Data_TypeExported_CtorExported_Product_All = Data_TypeExported_CtorExported_Product_All
data Data_TypeExported_CtorExported_Product_ByName = Data_TypeExported_CtorExported_Product_ByName
data Data_TypeExported_CtorExported_Sum_All
  = Data_TypeExported_CtorExported_Sum_All_1
  | Data_TypeExported_CtorExported_Sum_All_2

data Data_TypeExported_CtorExported_Sum_ByName
  = Data_TypeExported_CtorExported_Sum_ByName_Exported
  | Data_TypeExported_CtorExported_Sum_ByName_NotExpored

type TypeAlias_TypeNotExported = Int
type TypeAlias_TypeExported = Int

newtype Newtype_TypeNotExported = Newtype_TypeNotExported Int
newtype Newtype_TypeOnlyExported = Newtype_TypeOnlyExported Int
newtype Newtype_TypeExported_CtorExported_All = Newtype_TypeExported_CtorExported_All Int
newtype Newtype_TypeExported_CtorExported_ByName = Newtype_TypeExported_CtorExported_ByName Int

data EnsureSharedImportIsUsed =
  EnsureSharedImportIsUsed
    (Shared.SharedMyData Int Int Int) (Shared.SharedMyAlias String) (Shared.SharedMyNewtype Int Int Int)

usePrelude :: String
usePrelude = show 1
