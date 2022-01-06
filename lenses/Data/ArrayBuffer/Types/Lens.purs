module Data.ArrayBuffer.Types.Lens where

import Data.ArrayBuffer.Types (ArrayView, ByteLength, ByteOffset, Float32, Float32Array, Float64, Float64Array, Int16, Int16Array, Int32, Int32Array, Int8, Int8Array, Uint16, Uint16Array, Uint32, Uint32Array, Uint8, Uint8Array, Uint8Clamped, Uint8ClampedArray)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_ByteOffset :: Iso' ByteOffset Int
_ByteOffset = identity

_ByteLength :: Iso' ByteLength Int
_ByteLength = identity

_Int8Array :: Iso' Int8Array (ArrayView Int8)
_Int8Array = identity

_Int16Array :: Iso' Int16Array (ArrayView Int16)
_Int16Array = identity

_Int32Array :: Iso' Int32Array (ArrayView Int32)
_Int32Array = identity

_Uint8Array :: Iso' Uint8Array (ArrayView Uint8)
_Uint8Array = identity

_Uint16Array :: Iso' Uint16Array (ArrayView Uint16)
_Uint16Array = identity

_Uint32Array :: Iso' Uint32Array (ArrayView Uint32)
_Uint32Array = identity

_Uint8ClampedArray :: Iso' Uint8ClampedArray (ArrayView Uint8Clamped)
_Uint8ClampedArray = identity

_Float32Array :: Iso' Float32Array (ArrayView Float32)
_Float32Array = identity

_Float64Array :: Iso' Float64Array (ArrayView Float64)
_Float64Array = identity
