module Tidy.Precedence.Lens where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Lens (Lens')
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (Prism', prism)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Prelude (Unit, const, identity, unit)
import PureScript.CST.Types (ModuleName, Operator)
import Tidy.Precedence (OperatorChain, Precedence, PrecedenceMap, OperatorNamespace(..), OperatorTree(..), QualifiedOperator(..))

_OperatorType :: Prism' OperatorNamespace Unit
_OperatorType = prism (const OperatorType) case _ of
  OperatorType -> Right unit
  other -> Left other

_OperatorValue :: Prism' OperatorNamespace Unit
_OperatorValue = prism (const OperatorValue) case _ of
  OperatorValue -> Right unit
  other -> Left other

_QualifiedOperator :: Lens' QualifiedOperator
  { arg1 :: (Maybe ModuleName), arg2 :: OperatorNamespace, arg3 :: Operator }
_QualifiedOperator = iso
  (\(QualifiedOperator arg1 arg2 arg3) -> { arg1: arg1, arg2: arg2, arg3: arg3 })
  \{ arg1, arg2, arg3 } -> QualifiedOperator arg1 arg2 arg3

_Precedence :: Iso' Precedence Int
_Precedence = identity

_PrecedenceMap :: Iso' PrecedenceMap
  (Map (Maybe ModuleName) (Map (Tuple OperatorNamespace Operator) Precedence))
_PrecedenceMap = identity

_OpList
  :: forall op a
   . Prism' (OperatorTree op a)
       { arg1 :: (OperatorTree op a), arg2 :: Precedence, arg3 :: (OperatorChain op a) }
_OpList = prism (\{ arg1, arg2, arg3 } -> OpList arg1 arg2 arg3) case _ of
  OpList arg1 arg2 arg3 -> Right { arg1: arg1, arg2: arg2, arg3: arg3 }
  other -> Left other

_OpPure :: forall op a. Prism' (OperatorTree op a) a
_OpPure = prism OpPure case _ of
  OpPure a -> Right a
  other -> Left other

_OperatorChain
  :: forall op a. Iso' (OperatorChain op a) (NonEmptyArray (Tuple op (OperatorTree op a)))
_OperatorChain = identity
