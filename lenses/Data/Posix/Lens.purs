module Data.Posix.Lens where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Posix (Gid, Pid, Uid)

_Pid :: Lens' Pid Int
_Pid = _Newtype

_Gid :: Lens' Gid Int
_Gid = _Newtype

_Uid :: Lens' Uid Int
_Uid = _Newtype
