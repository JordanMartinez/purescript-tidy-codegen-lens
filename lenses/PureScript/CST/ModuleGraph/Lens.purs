module PureScript.CST.ModuleGraph.Lens where

import Prelude

import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import PureScript.CST.ModuleGraph (ModuleSort(..))

_Sorted :: forall a. Prism' (ModuleSort a) (Array a)
_Sorted = prism Sorted case _ of
  Sorted a -> Right a
  other -> Left other

_CycleDetected :: forall a. Prism' (ModuleSort a) (Array a)
_CycleDetected = prism CycleDetected case _ of
  CycleDetected a -> Right a
  other -> Left other
