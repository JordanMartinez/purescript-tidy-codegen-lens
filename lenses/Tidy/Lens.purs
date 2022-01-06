module Tidy.Lens where

import Data.Either (Either(..))
import Data.Lens.Iso (Iso')
import Data.Lens.Prism (Prism', prism)
import Prelude (Unit, const, identity, unit)
import Tidy (Format, FormatOptions, ImportWrapOption(..), TypeArrowOption(..))
import Tidy.Doc (FormatDoc)
import Tidy.Precedence (PrecedenceMap)
import Tidy.Token (UnicodeOption)

_TypeArrowFirst :: Prism' TypeArrowOption Unit
_TypeArrowFirst = prism (const TypeArrowFirst) case _ of
  TypeArrowFirst -> Right unit
  other -> Left other

_TypeArrowLast :: Prism' TypeArrowOption Unit
_TypeArrowLast = prism (const TypeArrowLast) case _ of
  TypeArrowLast -> Right unit
  other -> Left other

_ImportWrapSource :: Prism' ImportWrapOption Unit
_ImportWrapSource = prism (const ImportWrapSource) case _ of
  ImportWrapSource -> Right unit
  other -> Left other

_ImportWrapAuto :: Prism' ImportWrapOption Unit
_ImportWrapAuto = prism (const ImportWrapAuto) case _ of
  ImportWrapAuto -> Right unit
  other -> Left other

_FormatOptions
  :: forall e a
   . Iso' (FormatOptions e a)
       { formatError :: e -> FormatDoc a
       , unicode :: UnicodeOption
       , typeArrowPlacement :: TypeArrowOption
       , operators :: PrecedenceMap
       , importWrap :: ImportWrapOption
       }
_FormatOptions = identity

_Format :: forall f e a. Iso' (Format f e a) (FormatOptions e a -> f -> FormatDoc a)
_Format = identity
