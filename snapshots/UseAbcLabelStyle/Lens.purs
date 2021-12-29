module Snapshots.UseAbcLabelStyle.Lens where

import Data.Either (Either(..))
import Data.Lens (Lens')
import Data.Lens.Lens (lens)
import Data.Lens.Prism (Prism', prism)
import Data.Lens.Record (prop)
import Prelude (Unit, const, unit)
import Type.Proxy (Proxy(..))

_Product
  :: forall a b c d e f g h
   . Lens' (Product a b c d e f g h)
       { a :: a, b :: b, c :: c, d :: d, e :: e, f :: f, g :: g, h :: h }
_Product = lens (\(Product a b c d e f g h) -> { a: a, b: b, c: c, d: d, e: e, f: f, g: g, h: h })
  \{ a, b, c, d, e, f, g, h } -> Product a b c d e f g h

_Ignore :: forall a b c d e f g h. Prism' (Sum a b c d e f g h) Unit
_Ignore = prism (const Ignore) case _ of
  Ignore -> Right unit
  other -> Left other

_Sum
  :: forall a b c d e f g h
   . Prism' (Sum a b c d e f g h) { a :: a, b :: b, c :: c, d :: d, e :: e, f :: f, g :: g, h :: h }
_Sum = prism (\{ a, b, c, d, e, f, g, h } -> Sum a b c d e f g h) case _ of
  Sum a b c d e f g h -> Right { a: a, b: b, c: c, d: d, e: e, f: f, g: g, h: h }
  other -> Left other

_propA :: forall r a. Lens' { a :: a | r } a
_propA = prop (Proxy :: Proxy "a")

_propB :: forall r a. Lens' { b :: a | r } a
_propB = prop (Proxy :: Proxy "b")

_propC :: forall r a. Lens' { c :: a | r } a
_propC = prop (Proxy :: Proxy "c")

_propD :: forall r a. Lens' { d :: a | r } a
_propD = prop (Proxy :: Proxy "d")

_propE :: forall r a. Lens' { e :: a | r } a
_propE = prop (Proxy :: Proxy "e")

_propF :: forall r a. Lens' { f :: a | r } a
_propF = prop (Proxy :: Proxy "f")

_propG :: forall r a. Lens' { g :: a | r } a
_propG = prop (Proxy :: Proxy "g")

_propH :: forall r a. Lens' { h :: a | r } a
_propH = prop (Proxy :: Proxy "h")
