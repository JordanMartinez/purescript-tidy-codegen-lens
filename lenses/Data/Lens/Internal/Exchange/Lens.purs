module Data.Lens.Internal.Exchange.Lens where

import Data.Lens (Lens')
import Data.Lens.Internal.Exchange (Exchange(..))
import Data.Lens.Iso (iso)
import Data.Tuple (Tuple(..))

_Exchange :: forall a b s t. Lens' (Exchange a b s t) (Tuple (s -> a) (b -> t))
_Exchange = iso (\(Exchange a b) -> Tuple a b) \(Tuple a b) -> Exchange a b
