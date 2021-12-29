module Snapshots.Imports.ImportedExplicitTypesWithModuleAlias where

data MyData a b c = MyData a b c

newtype MyNewtype :: Type -> Type -> Type -> Type
newtype MyNewtype a b c = MyNewtype String

type MyAlias a = Array a
