module Control.Monad.List.Trans.Lens where

import Control.Monad.List.Trans (ListT, Step(..))
import Data.Either (Either(..))
import Data.Lazy (Lazy)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Prism (Prism', prism)
import Data.Tuple (Tuple(..))
import Prelude (Unit, const, unit)

_ListT :: forall f a. Lens' (ListT f a) (f (Step a (ListT f a)))
_ListT = _Newtype

_Yield :: forall a s. Prism' (Step a s) (Tuple a (Lazy s))
_Yield = prism (\(Tuple a b) -> Yield a b) case _ of
  Yield a b -> Right (Tuple a b)
  other -> Left other

_Skip :: forall a s. Prism' (Step a s) (Lazy s)
_Skip = prism Skip case _ of
  Skip a -> Right a
  other -> Left other

_Done :: forall a s. Prism' (Step a s) Unit
_Done = prism (const Done) case _ of
  Done -> Right unit
  other -> Left other
