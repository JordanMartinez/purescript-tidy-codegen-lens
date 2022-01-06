module Node.Buffer.Types.Lens where

import Prelude

import Data.Either (Either(..))
import Data.Lens.Iso (Iso')
import Data.Lens.Prism (Prism', prism)
import Node.Buffer.Types (Octet, Offset, BufferValueType(..))
import Prelude (Unit, const, identity, unit)

_Octet :: Iso' Octet Int
_Octet = identity

_Offset :: Iso' Offset Int
_Offset = identity

_UInt8 :: Prism' BufferValueType Unit
_UInt8 = prism (const UInt8) case _ of
  UInt8 -> Right unit
  other -> Left other

_UInt16LE :: Prism' BufferValueType Unit
_UInt16LE = prism (const UInt16LE) case _ of
  UInt16LE -> Right unit
  other -> Left other

_UInt16BE :: Prism' BufferValueType Unit
_UInt16BE = prism (const UInt16BE) case _ of
  UInt16BE -> Right unit
  other -> Left other

_UInt32LE :: Prism' BufferValueType Unit
_UInt32LE = prism (const UInt32LE) case _ of
  UInt32LE -> Right unit
  other -> Left other

_UInt32BE :: Prism' BufferValueType Unit
_UInt32BE = prism (const UInt32BE) case _ of
  UInt32BE -> Right unit
  other -> Left other

_Int8 :: Prism' BufferValueType Unit
_Int8 = prism (const Int8) case _ of
  Int8 -> Right unit
  other -> Left other

_Int16LE :: Prism' BufferValueType Unit
_Int16LE = prism (const Int16LE) case _ of
  Int16LE -> Right unit
  other -> Left other

_Int16BE :: Prism' BufferValueType Unit
_Int16BE = prism (const Int16BE) case _ of
  Int16BE -> Right unit
  other -> Left other

_Int32LE :: Prism' BufferValueType Unit
_Int32LE = prism (const Int32LE) case _ of
  Int32LE -> Right unit
  other -> Left other

_Int32BE :: Prism' BufferValueType Unit
_Int32BE = prism (const Int32BE) case _ of
  Int32BE -> Right unit
  other -> Left other

_FloatLE :: Prism' BufferValueType Unit
_FloatLE = prism (const FloatLE) case _ of
  FloatLE -> Right unit
  other -> Left other

_FloatBE :: Prism' BufferValueType Unit
_FloatBE = prism (const FloatBE) case _ of
  FloatBE -> Right unit
  other -> Left other

_DoubleLE :: Prism' BufferValueType Unit
_DoubleLE = prism (const DoubleLE) case _ of
  DoubleLE -> Right unit
  other -> Left other

_DoubleBE :: Prism' BufferValueType Unit
_DoubleBE = prism (const DoubleBE) case _ of
  DoubleBE -> Right unit
  other -> Left other
