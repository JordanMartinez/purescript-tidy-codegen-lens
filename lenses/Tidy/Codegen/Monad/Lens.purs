module Tidy.Codegen.Monad.Lens where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Lens (Lens')
import Data.Lens.Iso (Iso', iso)
import Data.Lens.Prism (Prism', prism)
import Data.List (List)
import Data.Map (Map)
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Prelude (identity)
import PureScript.CST.Types (Declaration, Ident, ModuleName, Proper, QualifiedName)
import Tidy.Codegen.Monad (Codegen, CodegenState, CodegenT, CodegenExport(..), CodegenImport(..), ImportName(..))
import Tidy.Codegen.Types (SymbolName)

_CodegenExportType :: Prism' CodegenExport (Tuple Boolean Proper)
_CodegenExportType = prism (\(Tuple a b) -> CodegenExportType a b) case _ of
  CodegenExportType a b -> Right (Tuple a b)
  other -> Left other

_CodegenExportTypeOp :: Prism' CodegenExport SymbolName
_CodegenExportTypeOp = prism CodegenExportTypeOp case _ of
  CodegenExportTypeOp a -> Right a
  other -> Left other

_CodegenExportClass :: Prism' CodegenExport Proper
_CodegenExportClass = prism CodegenExportClass case _ of
  CodegenExportClass a -> Right a
  other -> Left other

_CodegenExportValue :: Prism' CodegenExport Ident
_CodegenExportValue = prism CodegenExportValue case _ of
  CodegenExportValue a -> Right a
  other -> Left other

_CodegenExportOp :: Prism' CodegenExport SymbolName
_CodegenExportOp = prism CodegenExportOp case _ of
  CodegenExportOp a -> Right a
  other -> Left other

_CodegenExportModule :: Prism' CodegenExport ModuleName
_CodegenExportModule = prism CodegenExportModule case _ of
  CodegenExportModule a -> Right a
  other -> Left other

_CodegenImportType :: Prism' CodegenImport (Tuple Boolean Proper)
_CodegenImportType = prism (\(Tuple a b) -> CodegenImportType a b) case _ of
  CodegenImportType a b -> Right (Tuple a b)
  other -> Left other

_CodegenImportTypeOp :: Prism' CodegenImport SymbolName
_CodegenImportTypeOp = prism CodegenImportTypeOp case _ of
  CodegenImportTypeOp a -> Right a
  other -> Left other

_CodegenImportClass :: Prism' CodegenImport Proper
_CodegenImportClass = prism CodegenImportClass case _ of
  CodegenImportClass a -> Right a
  other -> Left other

_CodegenImportValue :: Prism' CodegenImport Ident
_CodegenImportValue = prism CodegenImportValue case _ of
  CodegenImportValue a -> Right a
  other -> Left other

_CodegenImportOp :: Prism' CodegenImport SymbolName
_CodegenImportOp = prism CodegenImportOp case _ of
  CodegenImportOp a -> Right a
  other -> Left other

_CodegenState
  :: forall e
   . Iso' (CodegenState e)
       { exports :: Set CodegenExport
       , importsOpen :: Set ModuleName
       , importsHiding :: Map ModuleName (Set CodegenImport)
       , importsHidingQualified :: Map ModuleName (Map ModuleName (Set CodegenImport))
       , importsFrom :: Map ModuleName (Set (CodegenImport))
       , importsQualified :: Map ModuleName (Set ModuleName)
       , declarations :: List (Declaration e)
       }
_CodegenState = identity

_Codegen :: forall e. Iso' (Codegen e) (CodegenT e (Free Identity))
_Codegen = identity

_ImportName :: forall name. Lens' (ImportName name) (Tuple CodegenImport (QualifiedName name))
_ImportName = iso (\(ImportName a b) -> Tuple a b) \(Tuple a b) -> ImportName a b
