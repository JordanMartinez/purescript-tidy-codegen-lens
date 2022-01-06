module Data.CodePoint.Unicode.Internal.Casing.Lens where

import Prelude

import Data.CodePoint.Unicode.Internal.Casing (CaseRec)
import Data.Lens.Iso (Iso')
import Prelude (identity)

_CaseRec :: Iso' CaseRec
  { code :: Int
  , lower :: Array Int
  , title :: Array Int
  , upper :: Array Int
  , fold :: Int
  , foldFull :: Array Int
  }
_CaseRec = identity
