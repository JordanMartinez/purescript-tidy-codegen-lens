module Data.List.Types.Lens where

import Data.Either (Either(..))
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Prism (Prism', prism)
import Data.List.Types (NonEmptyList, List(..))
import Data.NonEmpty (NonEmpty)
import Data.Tuple (Tuple(..))
import Prelude (Unit, const, unit)

_Nil :: forall a. Prism' (List a) Unit
_Nil = prism (const Nil) case _ of
  Nil -> Right unit
  other -> Left other

_Cons :: forall a. Prism' (List a) (Tuple a (List a))
_Cons = prism (\(Tuple a b) -> Cons a b) case _ of
  Cons a b -> Right (Tuple a b)
  other -> Left other

_NonEmptyList :: forall a. Lens' (NonEmptyList a) (NonEmpty List a)
_NonEmptyList = _Newtype
