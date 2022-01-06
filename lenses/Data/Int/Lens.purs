module Data.Int.Lens where

import Data.Either (Either(..))
import Data.Int (Parity(..))
import Data.Lens.Prism (Prism', prism)
import Prelude (Unit, const, unit)

_Even :: Prism' Parity Unit
_Even = prism (const Even) case _ of
  Even -> Right unit
  other -> Left other

_Odd :: Prism' Parity Unit
_Odd = prism (const Odd) case _ of
  Odd -> Right unit
  other -> Left other
