module Tidy.Codegen.Lens where

import Prelude

import Data.Lazy (Lazy)
import Data.Lens.Iso (Iso')
import Prelude (identity)
import Tidy (ImportWrapOption, TypeArrowOption, UnicodeOption)
import Tidy.Codegen (PrintOptions)
import Tidy.Precedence (PrecedenceMap)

_PrintOptions :: Iso' PrintOptions
  { importWrap :: ImportWrapOption
  , indentUnit :: String
  , indentWidth :: Int
  , operators :: Lazy PrecedenceMap
  , pageWidth :: Int
  , ribbonRatio :: Number
  , typeArrowPlacement :: TypeArrowOption
  , unicode :: UnicodeOption
  }
_PrintOptions = identity
