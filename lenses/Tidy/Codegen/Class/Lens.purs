module Tidy.Codegen.Class.Lens where

import Prelude

import Data.Lens.Iso (Iso')
import Prelude (identity)
import PureScript.CST.Types (Comment, LineFeed)
import Tidy.Codegen.Class (LeadingComments, TrailingComments)

_LeadingComments
  :: forall r. Iso' (LeadingComments r) (leadingComments :: Array (Comment LineFeed) | r)
_LeadingComments = identity

_TrailingComments
  :: forall trl r. Iso' (TrailingComments trl r) (trailingComments :: Array (Comment trl) | r)
_TrailingComments = identity
