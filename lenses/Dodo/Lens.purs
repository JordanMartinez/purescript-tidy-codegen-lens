module Dodo.Lens where

import Prelude

import Data.Lens.Iso (Iso')
import Dodo (PrintOptions)
import Prelude (identity)

_PrintOptions :: Iso' PrintOptions
  { pageWidth :: Int
  , ribbonRatio :: Number
  , indentUnit :: String
  , indentWidth :: Int
  }
_PrintOptions = identity
