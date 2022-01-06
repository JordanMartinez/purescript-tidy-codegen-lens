module Data.Generic.Rep.Lens where

import Data.Either (Either(..))
import Data.Generic.Rep (NoArguments(..), Product(..), Sum(..))
import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.Lens.Prism (Prism', prism)
import Data.Tuple (Tuple(..))
import Prelude (Unit, const, unit)

_NoArguments :: Lens' NoArguments Unit
_NoArguments = iso (const unit) (const NoArguments)

_Inl :: forall a b. Prism' (Sum a b) a
_Inl = prism Inl case _ of
  Inl a -> Right a
  other -> Left other

_Inr :: forall a b. Prism' (Sum a b) b
_Inr = prism Inr case _ of
  Inr a -> Right a
  other -> Left other

_Product :: forall a b. Lens' (Product a b) (Tuple a b)
_Product = iso (\(Product a b) -> Tuple a b) \(Tuple a b) -> Product a b
