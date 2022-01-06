module Data.CodePoint.Unicode.Internal.Lens where

import Data.CodePoint.Unicode.Internal (UnicodeCategory(..))
import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Prelude (Unit, const, unit)

_NUMCAT_LU :: Prism' UnicodeCategory Unit
_NUMCAT_LU = prism (const NUMCAT_LU) case _ of
  NUMCAT_LU -> Right unit
  other -> Left other

_NUMCAT_LL :: Prism' UnicodeCategory Unit
_NUMCAT_LL = prism (const NUMCAT_LL) case _ of
  NUMCAT_LL -> Right unit
  other -> Left other

_NUMCAT_LT :: Prism' UnicodeCategory Unit
_NUMCAT_LT = prism (const NUMCAT_LT) case _ of
  NUMCAT_LT -> Right unit
  other -> Left other

_NUMCAT_LM :: Prism' UnicodeCategory Unit
_NUMCAT_LM = prism (const NUMCAT_LM) case _ of
  NUMCAT_LM -> Right unit
  other -> Left other

_NUMCAT_LO :: Prism' UnicodeCategory Unit
_NUMCAT_LO = prism (const NUMCAT_LO) case _ of
  NUMCAT_LO -> Right unit
  other -> Left other

_NUMCAT_MN :: Prism' UnicodeCategory Unit
_NUMCAT_MN = prism (const NUMCAT_MN) case _ of
  NUMCAT_MN -> Right unit
  other -> Left other

_NUMCAT_MC :: Prism' UnicodeCategory Unit
_NUMCAT_MC = prism (const NUMCAT_MC) case _ of
  NUMCAT_MC -> Right unit
  other -> Left other

_NUMCAT_ME :: Prism' UnicodeCategory Unit
_NUMCAT_ME = prism (const NUMCAT_ME) case _ of
  NUMCAT_ME -> Right unit
  other -> Left other

_NUMCAT_ND :: Prism' UnicodeCategory Unit
_NUMCAT_ND = prism (const NUMCAT_ND) case _ of
  NUMCAT_ND -> Right unit
  other -> Left other

_NUMCAT_NL :: Prism' UnicodeCategory Unit
_NUMCAT_NL = prism (const NUMCAT_NL) case _ of
  NUMCAT_NL -> Right unit
  other -> Left other

_NUMCAT_NO :: Prism' UnicodeCategory Unit
_NUMCAT_NO = prism (const NUMCAT_NO) case _ of
  NUMCAT_NO -> Right unit
  other -> Left other

_NUMCAT_PC :: Prism' UnicodeCategory Unit
_NUMCAT_PC = prism (const NUMCAT_PC) case _ of
  NUMCAT_PC -> Right unit
  other -> Left other

_NUMCAT_PD :: Prism' UnicodeCategory Unit
_NUMCAT_PD = prism (const NUMCAT_PD) case _ of
  NUMCAT_PD -> Right unit
  other -> Left other

_NUMCAT_PS :: Prism' UnicodeCategory Unit
_NUMCAT_PS = prism (const NUMCAT_PS) case _ of
  NUMCAT_PS -> Right unit
  other -> Left other

_NUMCAT_PE :: Prism' UnicodeCategory Unit
_NUMCAT_PE = prism (const NUMCAT_PE) case _ of
  NUMCAT_PE -> Right unit
  other -> Left other

_NUMCAT_PI :: Prism' UnicodeCategory Unit
_NUMCAT_PI = prism (const NUMCAT_PI) case _ of
  NUMCAT_PI -> Right unit
  other -> Left other

_NUMCAT_PF :: Prism' UnicodeCategory Unit
_NUMCAT_PF = prism (const NUMCAT_PF) case _ of
  NUMCAT_PF -> Right unit
  other -> Left other

_NUMCAT_PO :: Prism' UnicodeCategory Unit
_NUMCAT_PO = prism (const NUMCAT_PO) case _ of
  NUMCAT_PO -> Right unit
  other -> Left other

_NUMCAT_SM :: Prism' UnicodeCategory Unit
_NUMCAT_SM = prism (const NUMCAT_SM) case _ of
  NUMCAT_SM -> Right unit
  other -> Left other

_NUMCAT_SC :: Prism' UnicodeCategory Unit
_NUMCAT_SC = prism (const NUMCAT_SC) case _ of
  NUMCAT_SC -> Right unit
  other -> Left other

_NUMCAT_SK :: Prism' UnicodeCategory Unit
_NUMCAT_SK = prism (const NUMCAT_SK) case _ of
  NUMCAT_SK -> Right unit
  other -> Left other

_NUMCAT_SO :: Prism' UnicodeCategory Unit
_NUMCAT_SO = prism (const NUMCAT_SO) case _ of
  NUMCAT_SO -> Right unit
  other -> Left other

_NUMCAT_ZS :: Prism' UnicodeCategory Unit
_NUMCAT_ZS = prism (const NUMCAT_ZS) case _ of
  NUMCAT_ZS -> Right unit
  other -> Left other

_NUMCAT_ZL :: Prism' UnicodeCategory Unit
_NUMCAT_ZL = prism (const NUMCAT_ZL) case _ of
  NUMCAT_ZL -> Right unit
  other -> Left other

_NUMCAT_ZP :: Prism' UnicodeCategory Unit
_NUMCAT_ZP = prism (const NUMCAT_ZP) case _ of
  NUMCAT_ZP -> Right unit
  other -> Left other

_NUMCAT_CC :: Prism' UnicodeCategory Unit
_NUMCAT_CC = prism (const NUMCAT_CC) case _ of
  NUMCAT_CC -> Right unit
  other -> Left other

_NUMCAT_CF :: Prism' UnicodeCategory Unit
_NUMCAT_CF = prism (const NUMCAT_CF) case _ of
  NUMCAT_CF -> Right unit
  other -> Left other

_NUMCAT_CS :: Prism' UnicodeCategory Unit
_NUMCAT_CS = prism (const NUMCAT_CS) case _ of
  NUMCAT_CS -> Right unit
  other -> Left other

_NUMCAT_CO :: Prism' UnicodeCategory Unit
_NUMCAT_CO = prism (const NUMCAT_CO) case _ of
  NUMCAT_CO -> Right unit
  other -> Left other

_NUMCAT_CN :: Prism' UnicodeCategory Unit
_NUMCAT_CN = prism (const NUMCAT_CN) case _ of
  NUMCAT_CN -> Right unit
  other -> Left other
