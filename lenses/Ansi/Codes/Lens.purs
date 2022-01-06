module Ansi.Codes.Lens where

import Prelude

import Ansi.Codes (Color(..), EraseParam(..), EscapeCode(..), GraphicsParam(..), RenderingMode(..))
import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Data.List.NonEmpty (NonEmptyList)
import Data.Tuple (Tuple(..))
import Prelude (Unit, const, unit)

_Up :: Prism' EscapeCode Int
_Up = prism Up case _ of
  Up a -> Right a
  other -> Left other

_Down :: Prism' EscapeCode Int
_Down = prism Down case _ of
  Down a -> Right a
  other -> Left other

_Forward :: Prism' EscapeCode Int
_Forward = prism Forward case _ of
  Forward a -> Right a
  other -> Left other

_Back :: Prism' EscapeCode Int
_Back = prism Back case _ of
  Back a -> Right a
  other -> Left other

_NextLine :: Prism' EscapeCode Int
_NextLine = prism NextLine case _ of
  NextLine a -> Right a
  other -> Left other

_PreviousLine :: Prism' EscapeCode Int
_PreviousLine = prism PreviousLine case _ of
  PreviousLine a -> Right a
  other -> Left other

_HorizontalAbsolute :: Prism' EscapeCode Int
_HorizontalAbsolute = prism HorizontalAbsolute case _ of
  HorizontalAbsolute a -> Right a
  other -> Left other

_Position :: Prism' EscapeCode (Tuple Int Int)
_Position = prism (\(Tuple a b) -> Position a b) case _ of
  Position a b -> Right (Tuple a b)
  other -> Left other

_EraseData :: Prism' EscapeCode EraseParam
_EraseData = prism EraseData case _ of
  EraseData a -> Right a
  other -> Left other

_EraseLine :: Prism' EscapeCode EraseParam
_EraseLine = prism EraseLine case _ of
  EraseLine a -> Right a
  other -> Left other

_ScrollUp :: Prism' EscapeCode Int
_ScrollUp = prism ScrollUp case _ of
  ScrollUp a -> Right a
  other -> Left other

_ScrollDown :: Prism' EscapeCode Int
_ScrollDown = prism ScrollDown case _ of
  ScrollDown a -> Right a
  other -> Left other

_Graphics :: Prism' EscapeCode (NonEmptyList GraphicsParam)
_Graphics = prism Graphics case _ of
  Graphics a -> Right a
  other -> Left other

_SavePosition :: Prism' EscapeCode Unit
_SavePosition = prism (const SavePosition) case _ of
  SavePosition -> Right unit
  other -> Left other

_RestorePosition :: Prism' EscapeCode Unit
_RestorePosition = prism (const RestorePosition) case _ of
  RestorePosition -> Right unit
  other -> Left other

_QueryPosition :: Prism' EscapeCode Unit
_QueryPosition = prism (const QueryPosition) case _ of
  QueryPosition -> Right unit
  other -> Left other

_HideCursor :: Prism' EscapeCode Unit
_HideCursor = prism (const HideCursor) case _ of
  HideCursor -> Right unit
  other -> Left other

_ShowCursor :: Prism' EscapeCode Unit
_ShowCursor = prism (const ShowCursor) case _ of
  ShowCursor -> Right unit
  other -> Left other

_ToEnd :: Prism' EraseParam Unit
_ToEnd = prism (const ToEnd) case _ of
  ToEnd -> Right unit
  other -> Left other

_FromBeginning :: Prism' EraseParam Unit
_FromBeginning = prism (const FromBeginning) case _ of
  FromBeginning -> Right unit
  other -> Left other

_Entire :: Prism' EraseParam Unit
_Entire = prism (const Entire) case _ of
  Entire -> Right unit
  other -> Left other

_Reset :: Prism' GraphicsParam Unit
_Reset = prism (const Reset) case _ of
  Reset -> Right unit
  other -> Left other

_PMode :: Prism' GraphicsParam RenderingMode
_PMode = prism PMode case _ of
  PMode a -> Right a
  other -> Left other

_PForeground :: Prism' GraphicsParam Color
_PForeground = prism PForeground case _ of
  PForeground a -> Right a
  other -> Left other

_PBackground :: Prism' GraphicsParam Color
_PBackground = prism PBackground case _ of
  PBackground a -> Right a
  other -> Left other

_Bold :: Prism' RenderingMode Unit
_Bold = prism (const Bold) case _ of
  Bold -> Right unit
  other -> Left other

_Dim :: Prism' RenderingMode Unit
_Dim = prism (const Dim) case _ of
  Dim -> Right unit
  other -> Left other

_Italic :: Prism' RenderingMode Unit
_Italic = prism (const Italic) case _ of
  Italic -> Right unit
  other -> Left other

_Underline :: Prism' RenderingMode Unit
_Underline = prism (const Underline) case _ of
  Underline -> Right unit
  other -> Left other

_Inverse :: Prism' RenderingMode Unit
_Inverse = prism (const Inverse) case _ of
  Inverse -> Right unit
  other -> Left other

_Strikethrough :: Prism' RenderingMode Unit
_Strikethrough = prism (const Strikethrough) case _ of
  Strikethrough -> Right unit
  other -> Left other

_Black :: Prism' Color Unit
_Black = prism (const Black) case _ of
  Black -> Right unit
  other -> Left other

_Red :: Prism' Color Unit
_Red = prism (const Red) case _ of
  Red -> Right unit
  other -> Left other

_Green :: Prism' Color Unit
_Green = prism (const Green) case _ of
  Green -> Right unit
  other -> Left other

_Yellow :: Prism' Color Unit
_Yellow = prism (const Yellow) case _ of
  Yellow -> Right unit
  other -> Left other

_Blue :: Prism' Color Unit
_Blue = prism (const Blue) case _ of
  Blue -> Right unit
  other -> Left other

_Magenta :: Prism' Color Unit
_Magenta = prism (const Magenta) case _ of
  Magenta -> Right unit
  other -> Left other

_Cyan :: Prism' Color Unit
_Cyan = prism (const Cyan) case _ of
  Cyan -> Right unit
  other -> Left other

_White :: Prism' Color Unit
_White = prism (const White) case _ of
  White -> Right unit
  other -> Left other

_BrightBlack :: Prism' Color Unit
_BrightBlack = prism (const BrightBlack) case _ of
  BrightBlack -> Right unit
  other -> Left other

_BrightRed :: Prism' Color Unit
_BrightRed = prism (const BrightRed) case _ of
  BrightRed -> Right unit
  other -> Left other

_BrightGreen :: Prism' Color Unit
_BrightGreen = prism (const BrightGreen) case _ of
  BrightGreen -> Right unit
  other -> Left other

_BrightYellow :: Prism' Color Unit
_BrightYellow = prism (const BrightYellow) case _ of
  BrightYellow -> Right unit
  other -> Left other

_BrightBlue :: Prism' Color Unit
_BrightBlue = prism (const BrightBlue) case _ of
  BrightBlue -> Right unit
  other -> Left other

_BrightMagenta :: Prism' Color Unit
_BrightMagenta = prism (const BrightMagenta) case _ of
  BrightMagenta -> Right unit
  other -> Left other

_BrightCyan :: Prism' Color Unit
_BrightCyan = prism (const BrightCyan) case _ of
  BrightCyan -> Right unit
  other -> Left other

_BrightWhite :: Prism' Color Unit
_BrightWhite = prism (const BrightWhite) case _ of
  BrightWhite -> Right unit
  other -> Left other
