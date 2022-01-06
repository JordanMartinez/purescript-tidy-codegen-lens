module Node.Platform.Lens where

import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Node.Platform (Platform(..))
import Prelude (Unit, const, unit)

_AIX :: Prism' Platform Unit
_AIX = prism (const AIX) case _ of
  AIX -> Right unit
  other -> Left other

_Darwin :: Prism' Platform Unit
_Darwin = prism (const Darwin) case _ of
  Darwin -> Right unit
  other -> Left other

_FreeBSD :: Prism' Platform Unit
_FreeBSD = prism (const FreeBSD) case _ of
  FreeBSD -> Right unit
  other -> Left other

_Linux :: Prism' Platform Unit
_Linux = prism (const Linux) case _ of
  Linux -> Right unit
  other -> Left other

_OpenBSD :: Prism' Platform Unit
_OpenBSD = prism (const OpenBSD) case _ of
  OpenBSD -> Right unit
  other -> Left other

_SunOS :: Prism' Platform Unit
_SunOS = prism (const SunOS) case _ of
  SunOS -> Right unit
  other -> Left other

_Win32 :: Prism' Platform Unit
_Win32 = prism (const Win32) case _ of
  Win32 -> Right unit
  other -> Left other

_Android :: Prism' Platform Unit
_Android = prism (const Android) case _ of
  Android -> Right unit
  other -> Left other
