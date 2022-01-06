module PureScript.CST.Print.Lens where

import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Prelude (Unit, const, unit)
import PureScript.CST.Print (TokenOption(..))

_ShowLayout :: Prism' TokenOption Unit
_ShowLayout = prism (const ShowLayout) case _ of
  ShowLayout -> Right unit
  other -> Left other

_HideLayout :: Prism' TokenOption Unit
_HideLayout = prism (const HideLayout) case _ of
  HideLayout -> Right unit
  other -> Left other
