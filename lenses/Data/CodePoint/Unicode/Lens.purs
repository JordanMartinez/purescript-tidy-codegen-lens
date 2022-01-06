module Data.CodePoint.Unicode.Lens where

import Data.CodePoint.Unicode (GeneralCategory(..))
import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Prelude (Unit, const, unit)

_UppercaseLetter :: Prism' GeneralCategory Unit
_UppercaseLetter = prism (const UppercaseLetter) case _ of
  UppercaseLetter -> Right unit
  other -> Left other

_LowercaseLetter :: Prism' GeneralCategory Unit
_LowercaseLetter = prism (const LowercaseLetter) case _ of
  LowercaseLetter -> Right unit
  other -> Left other

_TitlecaseLetter :: Prism' GeneralCategory Unit
_TitlecaseLetter = prism (const TitlecaseLetter) case _ of
  TitlecaseLetter -> Right unit
  other -> Left other

_ModifierLetter :: Prism' GeneralCategory Unit
_ModifierLetter = prism (const ModifierLetter) case _ of
  ModifierLetter -> Right unit
  other -> Left other

_OtherLetter :: Prism' GeneralCategory Unit
_OtherLetter = prism (const OtherLetter) case _ of
  OtherLetter -> Right unit
  other -> Left other

_NonSpacingMark :: Prism' GeneralCategory Unit
_NonSpacingMark = prism (const NonSpacingMark) case _ of
  NonSpacingMark -> Right unit
  other -> Left other

_SpacingCombiningMark :: Prism' GeneralCategory Unit
_SpacingCombiningMark = prism (const SpacingCombiningMark) case _ of
  SpacingCombiningMark -> Right unit
  other -> Left other

_EnclosingMark :: Prism' GeneralCategory Unit
_EnclosingMark = prism (const EnclosingMark) case _ of
  EnclosingMark -> Right unit
  other -> Left other

_DecimalNumber :: Prism' GeneralCategory Unit
_DecimalNumber = prism (const DecimalNumber) case _ of
  DecimalNumber -> Right unit
  other -> Left other

_LetterNumber :: Prism' GeneralCategory Unit
_LetterNumber = prism (const LetterNumber) case _ of
  LetterNumber -> Right unit
  other -> Left other

_OtherNumber :: Prism' GeneralCategory Unit
_OtherNumber = prism (const OtherNumber) case _ of
  OtherNumber -> Right unit
  other -> Left other

_ConnectorPunctuation :: Prism' GeneralCategory Unit
_ConnectorPunctuation = prism (const ConnectorPunctuation) case _ of
  ConnectorPunctuation -> Right unit
  other -> Left other

_DashPunctuation :: Prism' GeneralCategory Unit
_DashPunctuation = prism (const DashPunctuation) case _ of
  DashPunctuation -> Right unit
  other -> Left other

_OpenPunctuation :: Prism' GeneralCategory Unit
_OpenPunctuation = prism (const OpenPunctuation) case _ of
  OpenPunctuation -> Right unit
  other -> Left other

_ClosePunctuation :: Prism' GeneralCategory Unit
_ClosePunctuation = prism (const ClosePunctuation) case _ of
  ClosePunctuation -> Right unit
  other -> Left other

_InitialQuote :: Prism' GeneralCategory Unit
_InitialQuote = prism (const InitialQuote) case _ of
  InitialQuote -> Right unit
  other -> Left other

_FinalQuote :: Prism' GeneralCategory Unit
_FinalQuote = prism (const FinalQuote) case _ of
  FinalQuote -> Right unit
  other -> Left other

_OtherPunctuation :: Prism' GeneralCategory Unit
_OtherPunctuation = prism (const OtherPunctuation) case _ of
  OtherPunctuation -> Right unit
  other -> Left other

_MathSymbol :: Prism' GeneralCategory Unit
_MathSymbol = prism (const MathSymbol) case _ of
  MathSymbol -> Right unit
  other -> Left other

_CurrencySymbol :: Prism' GeneralCategory Unit
_CurrencySymbol = prism (const CurrencySymbol) case _ of
  CurrencySymbol -> Right unit
  other -> Left other

_ModifierSymbol :: Prism' GeneralCategory Unit
_ModifierSymbol = prism (const ModifierSymbol) case _ of
  ModifierSymbol -> Right unit
  other -> Left other

_OtherSymbol :: Prism' GeneralCategory Unit
_OtherSymbol = prism (const OtherSymbol) case _ of
  OtherSymbol -> Right unit
  other -> Left other

_Space :: Prism' GeneralCategory Unit
_Space = prism (const Space) case _ of
  Space -> Right unit
  other -> Left other

_LineSeparator :: Prism' GeneralCategory Unit
_LineSeparator = prism (const LineSeparator) case _ of
  LineSeparator -> Right unit
  other -> Left other

_ParagraphSeparator :: Prism' GeneralCategory Unit
_ParagraphSeparator = prism (const ParagraphSeparator) case _ of
  ParagraphSeparator -> Right unit
  other -> Left other

_Control :: Prism' GeneralCategory Unit
_Control = prism (const Control) case _ of
  Control -> Right unit
  other -> Left other

_Format :: Prism' GeneralCategory Unit
_Format = prism (const Format) case _ of
  Format -> Right unit
  other -> Left other

_Surrogate :: Prism' GeneralCategory Unit
_Surrogate = prism (const Surrogate) case _ of
  Surrogate -> Right unit
  other -> Left other

_PrivateUse :: Prism' GeneralCategory Unit
_PrivateUse = prism (const PrivateUse) case _ of
  PrivateUse -> Right unit
  other -> Left other

_NotAssigned :: Prism' GeneralCategory Unit
_NotAssigned = prism (const NotAssigned) case _ of
  NotAssigned -> Right unit
  other -> Left other
