module Snapshots.UseGlobalPropFile.Module1 where

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

type TypeAlias_Record =
  { foo :: Int
  , bar :: String
  , baz :: Boolean
  }

type TypeAlias_Type = String

newtype NewtypedRecord = NewtypedRecord
  { first :: String
  , second :: String
  }

newtype NewtypedType = NewtypedType Int

type SameLabelsAsOtherModule =
  { quarts :: String
  , bumblebee :: String
  , "力" :: String
  }
