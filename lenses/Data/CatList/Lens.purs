module Data.CatList.Lens where

import Data.CatList (CatList(..))
import Data.CatQueue as Q
import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Data.Tuple (Tuple(..))
import Prelude (Unit, const, unit)

_CatNil :: forall a. Prism' (CatList a) Unit
_CatNil = prism (const CatNil) case _ of
  CatNil -> Right unit
  other -> Left other

_CatCons :: forall a. Prism' (CatList a) (Tuple a (Q.CatQueue (CatList a)))
_CatCons = prism (\(Tuple a b) -> CatCons a b) case _ of
  CatCons a b -> Right (Tuple a b)
  other -> Left other
