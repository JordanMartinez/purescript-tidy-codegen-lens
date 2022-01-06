module Node.FS.Lens where

import Prelude

import Data.Either (Either(..))
import Data.Lens.Iso (Iso')
import Data.Lens.Prism (Prism', prism)
import Node.FS (BufferLength, BufferOffset, ByteCount, FileMode, FilePosition, FileFlags(..), SymlinkType(..))
import Prelude (Unit, const, identity, unit)

_R :: Prism' FileFlags Unit
_R = prism (const R) case _ of
  R -> Right unit
  other -> Left other

_R_PLUS :: Prism' FileFlags Unit
_R_PLUS = prism (const R_PLUS) case _ of
  R_PLUS -> Right unit
  other -> Left other

_RS :: Prism' FileFlags Unit
_RS = prism (const RS) case _ of
  RS -> Right unit
  other -> Left other

_RS_PLUS :: Prism' FileFlags Unit
_RS_PLUS = prism (const RS_PLUS) case _ of
  RS_PLUS -> Right unit
  other -> Left other

_W :: Prism' FileFlags Unit
_W = prism (const W) case _ of
  W -> Right unit
  other -> Left other

_WX :: Prism' FileFlags Unit
_WX = prism (const WX) case _ of
  WX -> Right unit
  other -> Left other

_W_PLUS :: Prism' FileFlags Unit
_W_PLUS = prism (const W_PLUS) case _ of
  W_PLUS -> Right unit
  other -> Left other

_WX_PLUS :: Prism' FileFlags Unit
_WX_PLUS = prism (const WX_PLUS) case _ of
  WX_PLUS -> Right unit
  other -> Left other

_A :: Prism' FileFlags Unit
_A = prism (const A) case _ of
  A -> Right unit
  other -> Left other

_AX :: Prism' FileFlags Unit
_AX = prism (const AX) case _ of
  AX -> Right unit
  other -> Left other

_A_PLUS :: Prism' FileFlags Unit
_A_PLUS = prism (const A_PLUS) case _ of
  A_PLUS -> Right unit
  other -> Left other

_AX_PLUS :: Prism' FileFlags Unit
_AX_PLUS = prism (const AX_PLUS) case _ of
  AX_PLUS -> Right unit
  other -> Left other

_FileMode :: Iso' FileMode Int
_FileMode = identity

_FilePosition :: Iso' FilePosition Int
_FilePosition = identity

_BufferLength :: Iso' BufferLength Int
_BufferLength = identity

_BufferOffset :: Iso' BufferOffset Int
_BufferOffset = identity

_ByteCount :: Iso' ByteCount Int
_ByteCount = identity

_FileLink :: Prism' SymlinkType Unit
_FileLink = prism (const FileLink) case _ of
  FileLink -> Right unit
  other -> Left other

_DirLink :: Prism' SymlinkType Unit
_DirLink = prism (const DirLink) case _ of
  DirLink -> Right unit
  other -> Left other

_JunctionLink :: Prism' SymlinkType Unit
_JunctionLink = prism (const JunctionLink) case _ of
  JunctionLink -> Right unit
  other -> Left other
