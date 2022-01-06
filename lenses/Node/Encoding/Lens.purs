module Node.Encoding.Lens where

import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Node.Encoding (Encoding(..))
import Prelude (Unit, const, unit)

_ASCII :: Prism' Encoding Unit
_ASCII = prism (const ASCII) case _ of
  ASCII -> Right unit
  other -> Left other

_UTF8 :: Prism' Encoding Unit
_UTF8 = prism (const UTF8) case _ of
  UTF8 -> Right unit
  other -> Left other

_UTF16LE :: Prism' Encoding Unit
_UTF16LE = prism (const UTF16LE) case _ of
  UTF16LE -> Right unit
  other -> Left other

_UCS2 :: Prism' Encoding Unit
_UCS2 = prism (const UCS2) case _ of
  UCS2 -> Right unit
  other -> Left other

_Base64 :: Prism' Encoding Unit
_Base64 = prism (const Base64) case _ of
  Base64 -> Right unit
  other -> Left other

_Latin1 :: Prism' Encoding Unit
_Latin1 = prism (const Latin1) case _ of
  Latin1 -> Right unit
  other -> Left other

_Binary :: Prism' Encoding Unit
_Binary = prism (const Binary) case _ of
  Binary -> Right unit
  other -> Left other

_Hex :: Prism' Encoding Unit
_Hex = prism (const Hex) case _ of
  Hex -> Right unit
  other -> Left other
