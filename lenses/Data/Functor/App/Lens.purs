module Data.Functor.App.Lens where

import Data.Functor.App (App)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)

_App :: forall f a. Lens' (App f a) (f a)
_App = _Newtype
