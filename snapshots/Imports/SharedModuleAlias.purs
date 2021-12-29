module Snapshots.Imports.SharedModuleAlias where

data SharedMyData a b c = MyData a b c

newtype SharedMyNewtype :: Type -> Type -> Type -> Type
newtype SharedMyNewtype a b c = SharedMyNewtype String

type SharedMyAlias a = Array a
