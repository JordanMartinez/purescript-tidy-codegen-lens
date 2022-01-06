module Dodo.Internal.Lens where

import Prelude

import Data.Either (Either(..))
import Data.Lens.Iso (Iso')
import Data.Lens.Prism (Prism', prism)
import Data.Tuple (Tuple(..))
import Dodo.Internal (LocalOptions, Position, Doc(..))
import Prelude (Unit, const, identity, unit)

_Position :: Iso' Position
  { line :: Int
  , column :: Int
  , indent :: Int
  , nextIndent :: Int
  , pageWidth :: Int
  , ribbonWidth :: Int
  }
_Position = identity

_Append :: forall a. Prism' (Doc a) (Tuple (Doc a) (Doc a))
_Append = prism (\(Tuple a b) -> Append a b) case _ of
  Append a b -> Right (Tuple a b)
  other -> Left other

_Indent :: forall a. Prism' (Doc a) (Doc a)
_Indent = prism Indent case _ of
  Indent a -> Right a
  other -> Left other

_Align :: forall a. Prism' (Doc a) (Tuple Int (Doc a))
_Align = prism (\(Tuple a b) -> Align a b) case _ of
  Align a b -> Right (Tuple a b)
  other -> Left other

_Annotate :: forall a. Prism' (Doc a) (Tuple a (Doc a))
_Annotate = prism (\(Tuple a b) -> Annotate a b) case _ of
  Annotate a b -> Right (Tuple a b)
  other -> Left other

_FlexSelect :: forall a. Prism' (Doc a) { arg1 :: (Doc a), arg2 :: (Doc a), arg3 :: (Doc a) }
_FlexSelect = prism (\{ arg1, arg2, arg3 } -> FlexSelect arg1 arg2 arg3) case _ of
  FlexSelect arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_FlexAlt :: forall a. Prism' (Doc a) (Tuple (Doc a) (Doc a))
_FlexAlt = prism (\(Tuple a b) -> FlexAlt a b) case _ of
  FlexAlt a b -> Right (Tuple a b)
  other -> Left other

_WithPosition :: forall a. Prism' (Doc a) (Position -> Doc a)
_WithPosition = prism WithPosition case _ of
  WithPosition a -> Right a
  other -> Left other

_Local :: forall a. Prism' (Doc a) (Tuple (LocalOptions -> LocalOptions) (Doc a))
_Local = prism (\(Tuple a b) -> Local a b) case _ of
  Local a b -> Right (Tuple a b)
  other -> Left other

_Text :: forall a. Prism' (Doc a) (Tuple Int String)
_Text = prism (\(Tuple a b) -> Text a b) case _ of
  Text a b -> Right (Tuple a b)
  other -> Left other

_Break :: forall a. Prism' (Doc a) Unit
_Break = prism (const Break) case _ of
  Break -> Right unit
  other -> Left other

_Empty :: forall a. Prism' (Doc a) Unit
_Empty = prism (const Empty) case _ of
  Empty -> Right unit
  other -> Left other

_LocalOptions :: Iso' LocalOptions
  { indent :: Int
  , indentSpaces :: String
  , indentUnit :: String
  , indentWidth :: Int
  , pageWidth :: Int
  , ribbonRatio :: Number
  }
_LocalOptions = identity
