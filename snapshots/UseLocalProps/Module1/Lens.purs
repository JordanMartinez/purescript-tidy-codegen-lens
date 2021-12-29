module Snapshots.UseLocalProps.Module1.Lens where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

_propBumblebee :: forall r a. Lens' { bumblebee :: a | r } a
_propBumblebee = prop (Proxy :: Proxy "bumblebee")

_propQuarts :: forall r a. Lens' { quarts :: a | r } a
_propQuarts = prop (Proxy :: Proxy "quarts")

_prop力 :: forall r a. Lens' { "力" :: a | r } a
_prop力 = prop (Proxy :: Proxy "力")
