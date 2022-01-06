module Data.List.ZipList.Lens where

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List.Lazy (List)
import Data.List.ZipList (ZipList)

_ZipList :: forall a. Lens' (ZipList a) (List a)
_ZipList = _Newtype
