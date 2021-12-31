module Snapshots.UseArgLabelStyle.Lens where

import Data.Either (Either(..))
import Data.Lens (Lens')
import Data.Lens.Iso (iso)
import Data.Lens.Prism (Prism', prism)
import Data.Lens.Record (prop)
import Prelude (Unit, const, unit)
import Snapshots.UseArgLabelStyle (Product(..), Sum(..))
import Type.Proxy (Proxy(..))

_Product
  :: forall a b c d e f g h
   . Lens' (Product a b c d e f g h)
       { arg1 :: a, arg2 :: b, arg3 :: c, arg4 :: d, arg5 :: e, arg6 :: f, arg7 :: g, arg8 :: h }
_Product = iso
  ( \(Product arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) ->
      { arg1: arg1
      , arg2: arg2
      , arg3: arg3
      , arg4: arg4
      , arg5: arg5
      , arg6: arg6
      , arg7: arg7
      , arg8: arg8
      }
  )
  \{ arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 } -> Product arg1 arg2 arg3 arg4 arg5 arg6 arg7
    arg8

_Ignore :: forall a b c d e f g h. Prism' (Sum a b c d e f g h) Unit
_Ignore = prism (const Ignore) case _ of
  Ignore -> Right unit
  other -> Left other

_Sum
  :: forall a b c d e f g h
   . Prism' (Sum a b c d e f g h)
       { arg1 :: a, arg2 :: b, arg3 :: c, arg4 :: d, arg5 :: e, arg6 :: f, arg7 :: g, arg8 :: h }
_Sum = prism
  ( \{ arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 } -> Sum arg1 arg2 arg3 arg4 arg5 arg6 arg7
      arg8
  )
  case _ of
    Sum arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 -> Right
      { arg1: arg1
      , arg2: arg2
      , arg3: arg3
      , arg4: arg4
      , arg5: arg5
      , arg6: arg6
      , arg7: arg7
      , arg8: arg8
      }
    other -> Left other

_propArg1 :: forall r a. Lens' { arg1 :: a | r } a
_propArg1 = prop (Proxy :: Proxy "arg1")

_propArg2 :: forall r a. Lens' { arg2 :: a | r } a
_propArg2 = prop (Proxy :: Proxy "arg2")

_propArg3 :: forall r a. Lens' { arg3 :: a | r } a
_propArg3 = prop (Proxy :: Proxy "arg3")

_propArg4 :: forall r a. Lens' { arg4 :: a | r } a
_propArg4 = prop (Proxy :: Proxy "arg4")

_propArg5 :: forall r a. Lens' { arg5 :: a | r } a
_propArg5 = prop (Proxy :: Proxy "arg5")

_propArg6 :: forall r a. Lens' { arg6 :: a | r } a
_propArg6 = prop (Proxy :: Proxy "arg6")

_propArg7 :: forall r a. Lens' { arg7 :: a | r } a
_propArg7 = prop (Proxy :: Proxy "arg7")

_propArg8 :: forall r a. Lens' { arg8 :: a | r } a
_propArg8 = prop (Proxy :: Proxy "arg8")
