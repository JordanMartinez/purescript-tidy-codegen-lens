module Snapshots.UseNoLabelPrefix where

import Data.Newtype (class Newtype)

data Foo = Foo { a :: Int, b :: String }

newtype Bar = Bar { height :: Number }

derive instance newtypeBar :: Newtype Bar _
