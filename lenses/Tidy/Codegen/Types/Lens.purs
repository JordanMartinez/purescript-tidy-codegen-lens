module Tidy.Codegen.Types.Lens where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Lens (Lens')
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Prelude (identity)
import PureScript.CST.Types (Ident, Labeled, ModuleName, Name, PatternGuard, Where)
import PureScript.CST.Types as CST
import Tidy.Codegen.Types (ClassMember, SourceString, SymbolName, GuardedBranch(..), Qualified(..))

_SymbolName :: Lens' SymbolName String
_SymbolName = _Newtype

_Qualified :: forall a. Lens' (Qualified a) (Tuple (Maybe ModuleName) a)
_Qualified = iso (\(Qualified a b) -> Tuple a b) \(Tuple a b) -> Qualified a b

_GuardedBranch
  :: forall e. Lens' (GuardedBranch e) (Tuple (NonEmptyArray (PatternGuard e)) (Where e))
_GuardedBranch = iso (\(GuardedBranch a b) -> Tuple a b) \(Tuple a b) -> GuardedBranch a b

_ClassMember :: forall e. Iso' (ClassMember e) (Labeled (Name Ident) (CST.Type e))
_ClassMember = identity

_SourceString :: Lens' SourceString String
_SourceString = _Newtype
