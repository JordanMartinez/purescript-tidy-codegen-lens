module Data.List.Lazy.Types.Lens where

import Data.Either (Either(..))
import Data.Lazy (Lazy)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Prism (Prism', prism)
import Data.List.Lazy.Types (List, NonEmptyList, Step(..))
import Data.NonEmpty (NonEmpty)
import Data.Tuple (Tuple(..))
import Prelude (Unit, const, unit)

_List :: forall a. Lens' (List a) (Lazy (Step a))
_List = _Newtype

_Nil :: forall a. Prism' (Step a) Unit
_Nil = prism (const Nil) case _ of
  Nil -> Right unit
  other -> Left other

_Cons :: forall a. Prism' (Step a) (Tuple a (List a))
_Cons = prism (\(Tuple a b) -> Cons a b) case _ of
  Cons a b -> Right (Tuple a b)
  other -> Left other

_NonEmptyList :: forall a. Lens' (NonEmptyList a) (Lazy (NonEmpty List a))
_NonEmptyList = _Newtype
