module MkLens where

import Prelude
import Prim hiding (Type, Row)

import CLI (CliArgs)
import Control.Alt ((<|>))
import Control.Monad.Free (runFree)
import Control.Monad.Writer (tell)
import Data.Array (fold, mapWithIndex)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Char (fromCharCode, toCharCode)
import Data.Identity (Identity(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String (splitAt, toUpper)
import Data.String.CodeUnits as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.Traversable (class Foldable, foldl, for, for_, traverse)
import Data.Tuple (Tuple(..), snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import MkDir as MkDir
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSA
import Node.Path (dirname)
import Partial.Unsafe (unsafePartial)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types (DataCtor(..), DataMembers(..), Declaration(..), Export(..), FixityOp(..), Foreign(..), Import(..), ImportDecl(..), Label(..), Labeled(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName, Name(..), Operator, Proper, QualifiedName(..), Row(..), Separated(..), Type(..), TypeVarBinding(..), Wrapped(..))
import Safe.Coerce (coerce)
import Tidy.Codegen (binderCtor, binderRecord, binderVar, caseBranch, declSignature, declValue, exprApp, exprCase, exprCtor, exprIdent, exprLambda, exprRecord, exprSection, exprTyped, printModule, typeApp, typeCtor, typeForall, typeRecord, typeString, typeVar)
import Tidy.Codegen.Monad (Codegen, importCtor, importFrom, importOpen, importOpenHiding, importType, importTypeAll, importTypeOp, importValue, runCodegenTModule)
import Types (RecordLabelStyle(..))

data ImportedTypeKey
  = ModuleAlias ModuleName
  | TypeName (Maybe ModuleName) Proper
  | TypeOperator (Maybe ModuleName) Operator

derive instance eqImportedTypeKey :: Eq ImportedTypeKey
derive instance ordImportedTypeKey :: Ord ImportedTypeKey

-- | Given a source file of...
-- | ```
-- | import Mod1 as Q
-- | import Mod2 (MyType1, type (+++)) as Q
-- | import Mod3 (MyType2, type (***))
-- |
-- | a = {} :: Q.Type
-- | b = {} :: Int Q./\ Int
-- | c = {} :: Q.MyType1
-- | d = {} :: Int Q.+++ Int
-- | e = {} :: MyType2
-- | f = {} :: Int *** Int
-- | ```
-- | then a value of this type would be
-- | ```
-- | Map.fromFoldable
-- |   [ Tuple (ModuleAlias $ ModuleName "Q") (ModuleName "Mod1")
-- |   , Tuple (TypeName (Just (ModuleName "Q")) (Proper "MyType")) (ModuleName "Mod2")
-- |   , Tuple (TypeOperator (Just (ModuleName "Q")) (Operator "/\")) (ModuleName "Mod2")
-- |   , Tuple (TypeName $ Proper "MyType") (ModuleName "Mod3")
-- |   , Tuple (TypeOperator $ Operator "/\") (ModuleName "Mod3")
-- |   ]
-- | ```
type ImportedTypes = Map ImportedTypeKey ModuleName

-- | Indicates the number of data constructors for a given type are exported.
-- | While the CST permits one to export specific constructors,
-- | one can only import all or 0 constructors.
data DataCtorMembers
  = DCMNone
  -- If we ever re-allow the ability to import only some constructors from a type
  -- then we should reuse this code
  -- | DCMSome (NonEmptySet Proper)
  | DCMAll

derive instance eqDataCtorMembers :: Eq DataCtorMembers
instance Semigroup DataCtorMembers where
  append = case _, _ of
    DCMAll, _ -> DCMAll
    _, DCMAll -> DCMAll
    -- If we ever re-allow the ability to import only some constructors from a type
    -- then we should reuse this code
    -- (DCMSome l) (DCMSome r) = DCMSome $ l <> r
    -- _ r@(DCMSome _) = r
    dcmNone, _ -> dcmNone

type ExportedTypeMap = Map Proper DataCtorMembers

type FileInfo =
  { inputFile :: String
  , outputFile :: String
  }

-- | Uses a two-step pass to generate all lenses/prisms for a file
-- | 1. Generate a lens/prism for a data type and newtype
-- | 2. Generate a lens for each label referenced in the previous generated
-- |    lenses or prisms.
generateLensModule :: CliArgs -> FileInfo -> Aff (Set String)
generateLensModule options { inputFile, outputFile } = do
  content <- FSA.readTextFile UTF8 inputFile
  case parseModule content of
    ParseSucceeded cst -> do
      let
        Tuple relevantTypes otherInfo = getDeclInfo cst
        modulePath = (unwrap otherInfo.sourceFileModName) <> ".Lens"
        Tuple labelNames generatedModule = runFree coerce $ unsafePartial $ runCodegenTModule modulePath do
          results <- traverse (genOptic options otherInfo) relevantTypes
          let
            { labelNames
            , unfoundRefTypes: Disj unfoundRefTypes
            } =
              foldl
                (\acc next -> { labelNames: Set.union acc.labelNames next.labelNames, unfoundRefTypes: acc.unfoundRefTypes <> next.unfoundRefTypes })
                { labelNames: Set.empty, unfoundRefTypes: Disj false }
                results
          when unfoundRefTypes do
            for_ otherInfo.openImports \modName ->
              importOpen modName
            for_ otherInfo.openHiddenImports reimportOpenHidingImports
          unless (isJust options.genGlobalPropFile) do
            genLensProp options.labelPrefix labelNames
          pure labelNames
      when (hasDecls generatedModule) do
        let
          outputtedContent = printModule generatedModule
          parentDir = dirname outputFile

        unlessM (FSA.exists parentDir) do
          MkDir.mkdirRecAff parentDir
        FSA.writeTextFile UTF8 outputFile outputtedContent
      pure labelNames
    _ ->
      liftEffect $ throw $
        "Parsing module for file path failed. Could not generate lens file for path: '" <> inputFile <> "'"
  where
  reimportOpenHidingImports :: _ -> _
  reimportOpenHidingImports { modName, modAlias, hiddenTypes } = do
    let
      qualifier = maybe "" (flip append "." <<< unwrap) modAlias
      qualify s = qualifier <> s
      modName' = unwrap modName
    for_ hiddenTypes case _ of
      ImportType tyName _ ->
        unsafePartial $ importOpenHiding modName' $ importType $ qualify $ unwrap $ unName tyName
      ImportTypeOp _ opName -> do
        unsafePartial $ importOpenHiding modName' $ importTypeOp $ qualify $ unwrap $ unName opName
      _ ->
        pure unit

type GenOpticInfo =
  { openImports :: Set ModuleName
  , openHiddenImports :: Array { modName :: ModuleName, modAlias :: Maybe ModuleName, hiddenTypes :: Array (Import Void) }
  , referencedTypes :: Map ImportedTypeKey ModuleName
  , sourceFileModName :: ModuleName
  }

type GenOpticResult =
  { labelNames :: Set String
  , unfoundRefTypes :: Disj Boolean
  }

-- | To generate an optic for a data type, the following must be true:
-- | - if a type alias, the type must be exported
-- | - if a data type, the type and all of its constructors must be exported
-- | - if a newtype, the newtype and its constructor must be exported. Moreover, it must have a derived `Newtype` instance
-- |
-- | Moreover, the generated file must also import all types that are referenced in the source file's types. Thus,
-- | we first determine what types are imported (besides those defined in the file), and later check
-- | which of those types are used in the source file's type declarations and then reimport those types.
-- |
-- | For example,
-- | ```
-- | module Example where
-- | import OpenModule1
-- | import OpenModule2 hiding (TypeFoo)
-- | import ClosedModule (Type1, Type2)
-- | import AliasedModule1 as Alias1
-- | import AliasedModule2 (Type3, Type4) as Alias2
-- | import AliasedModule3 hiding (TypeBar) as Alias3
-- |
-- | data MyFoo
-- |  = Constructor1 Alias1.SomeType -- forces us to import `Alias1`
-- |  | Constructor2 Type1 Alias2.Type3 -- import `ClosedModule (Type1)` and `Alias2 (Type3)`
-- |  | Constructor3 SomeType -- forces us to import all open imports since no idea where it's defined
-- |
-- | -- `Alias3` does not need to be imported because it's never referenced in `MyFoo`'s definition.
-- | ```
-- |
-- | We use `referencedTypes` to track what to import to ensure that a type referenced in `MyFoo`
-- | will be imported. If a referenced type cannot be found in `referencedTypes`, then it must be from an open import.
-- | We will import all the open imports only if this occurs.
getDeclInfo
  :: Module Void
  -> Tuple (Array DeclType) GenOpticInfo
getDeclInfo (Module { header: ModuleHeader { exports, imports, name: sourceFileModuleName }, body: ModuleBody { decls } }) =
  Tuple relevantTypes
    { openImports: importsInfo.openImports
    , openHiddenImports: importsInfo.openHiddenImports
    , referencedTypes: Map.union importsInfo.referencedTypes declsInfo.referencedTypes
    , sourceFileModName
    }
  where
  sourceFileModName = unName sourceFileModuleName

  -- The types for which we want to generate optics
  relevantTypes :: Array DeclType
  relevantTypes = declsInfo.types # Array.filter \{ tyName, keyword } -> case exportsInfo, keyword of
    Nothing, Type_AliasedType _ -> true
    Nothing, Data_Constructors _ -> true
    Nothing, Newtype_WrappedType _ -> Set.member tyName declsInfo.tysWithNewtypeInstances
    Just expMap, Type_AliasedType _ -> Map.member tyName expMap
    Just expMap, Data_Constructors _ -> maybe false (eq DCMAll) $ Map.lookup tyName expMap
    Just expMap, Newtype_WrappedType _ -> case Map.lookup tyName expMap of
        Nothing -> false
        Just DCMNone -> false
        Just DCMAll -> Set.member tyName declsInfo.tysWithNewtypeInstances

  exportsInfo :: Maybe ExportedTypeMap
  exportsInfo = map (foldl exportsFoldFn Map.empty <<< unWrappedSeparated) exports
    where
    exportsFoldFn :: ExportedTypeMap -> Export Void -> ExportedTypeMap
    exportsFoldFn acc = case _ of
      ExportType tyName members ->
        Map.alter
          case _ of
            Nothing -> Just $ extractMembers members
            Just dctors -> Just $ dctors <> extractMembers members
          (unName tyName)
          acc
      _ -> acc

    extractMembers :: Maybe DataMembers -> DataCtorMembers
    extractMembers = maybe DCMNone case _ of
      DataAll _ -> DCMAll
      DataEnumerated (Wrapped { value }) ->
        -- Previously, PureScript allowed you to export and import specific constructors.
        -- However, importing specific constructors was later removed.
        -- Thus, if one constructor was exported, the compiler will force all constructors
        -- to be exported.
        --
        -- If we reallow only a few constructors to be imported again, then we should use this code:
        -- value
        --   # maybe DCMNone (unSeparated
        --      >>> foldl (\acc next -> acc <> DCMAllDCMSome (NonEmptySet.singleton (unName next))) DCMNone)
        maybe DCMNone (const DCMAll) value

  importsInfo ::
    { openImports :: Set ModuleName
    , openHiddenImports :: Array { modName :: ModuleName, modAlias :: Maybe ModuleName, hiddenTypes :: Array (Import Void) }
    , referencedTypes :: Map ImportedTypeKey ModuleName
    }
  importsInfo = foldl importsFoldFn { openImports: Set.empty, openHiddenImports: [], referencedTypes: Map.empty } imports
    where
    importsFoldFn acc (ImportDecl r) = do
      case map (snd >>> unName) r.qualified, r.names of
        -- import Foo
        Nothing, Nothing ->
          acc
            { openImports = Set.insert (unName r.module) acc.openImports }

        -- import Foo as Q
        Just aliasModName, Nothing -> do
          acc
            { referencedTypes = Map.insert (ModuleAlias aliasModName) (unName r.module) acc.referencedTypes }

        -- import Foo hiding (Bar)
        Nothing, Just (Tuple (Just _) members) -> do
          let hiddenTypes = foldl extractHiddenTypes [] $ unWrappedSeparated members
          if Array.null hiddenTypes then do
            acc
              { openImports = Set.insert (unName r.module) acc.openImports }
          else do
            acc
              { openHiddenImports = Array.snoc acc.openHiddenImports
                  { modName: unName r.module
                  , modAlias: Nothing
                  , hiddenTypes
                  }
              }

        -- import Foo hiding (Bar) as B
        Just modAlias, Just (Tuple (Just _) members) -> do
          let hiddenTypes = foldl extractHiddenTypes [] $ unWrappedSeparated members
          if Array.null hiddenTypes then do
            acc
              { referencedTypes = Map.insert (ModuleAlias modAlias) (unName r.module) acc.referencedTypes
              , openImports = Set.insert modAlias acc.openImports
              }
          else do
            acc
              { referencedTypes = Map.insert (ModuleAlias modAlias) (unName r.module) acc.referencedTypes
              , openHiddenImports = Array.snoc acc.openHiddenImports
                  { modName: unName r.module
                  , modAlias: Just modAlias
                  , hiddenTypes
                  }
              }

        -- import Foo (Bar)
        -- import Foo (Bar) as Baz
        possibleModAlias, Just (Tuple Nothing members) -> do
          foldl insertType acc $ unWrappedSeparated members
          where
          insertType accum = case _ of
            ImportType tyName _->
              accum
                { referencedTypes = Map.insert (TypeName possibleModAlias (unName tyName)) (unName r.module) accum.referencedTypes }

            ImportTypeOp _ opName ->
              accum
                { referencedTypes = Map.insert (TypeOperator possibleModAlias (unName opName)) (unName r.module) accum.referencedTypes }
            _ ->
              accum

    extractHiddenTypes accum = case _ of
      x@(ImportType _ _) -> Array.snoc accum x
      x@(ImportTypeOp _ _) -> Array.snoc accum x
      _ -> accum

  declsInfo ::
    { types :: Array { tyName :: Proper, tyVars :: Array (TypeVarBinding Void), keyword :: DeclTypeKeyword }
    , referencedTypes :: ImportedTypes
    , tysWithNewtypeInstances :: Set Proper
    }
  declsInfo = foldl declsFoldFn { types: [], referencedTypes: Map.empty, tysWithNewtypeInstances: Set.empty } decls
    where
    declsFoldFn acc = case _ of
      DeclData ({ name, vars }) (Just (Tuple _ sep)) | Just ctors <- NEA.fromArray $ unSeparated sep -> do
        acc
          { types = Array.snoc acc.types $ { tyName: (unName name), tyVars: vars, keyword: Data_Constructors ctors }
          , referencedTypes = Map.insert (TypeName Nothing (unName name)) sourceFileModName acc.referencedTypes
          }
      DeclNewtype ({ name, vars }) _ _ ty -> do
        acc
          { types = Array.snoc acc.types $ { tyName: (unName name), tyVars: vars, keyword: Newtype_WrappedType ty }
          , referencedTypes = Map.insert (TypeName Nothing (unName name)) sourceFileModName acc.referencedTypes
          }
      DeclType ({ name, vars }) _ ty -> do
        acc
          { types = Array.snoc acc.types $ { tyName: (unName name), tyVars: vars, keyword: Type_AliasedType ty }
          , referencedTypes = Map.insert (TypeName Nothing (unName name)) sourceFileModName acc.referencedTypes
          }

      DeclForeign _ _ (ForeignData _ (Labeled { label })) -> do
        acc { referencedTypes = Map.insert (TypeName Nothing (unName label)) sourceFileModName acc.referencedTypes }
      DeclFixity { operator: FixityType _ _ _ opName } -> do
        acc { referencedTypes = Map.insert (TypeOperator Nothing (unName opName)) sourceFileModName acc.referencedTypes }

      -- derive instance Newtype TyNameNoTyVars _
      -- derive instance Newtype (TyNameWithTyVars a b c) _
      DeclDerive _ _ { className: QualifiedName r, types: [ tyCtorWithPossibleArgs, _underscore ] }
        | (unwrap $ r.name) == "Newtype" -- note: what if imported qualified? need to figure out what module alias is
        , Just tyCtor <- extractTypeCtor tyCtorWithPossibleArgs ->
            acc { tysWithNewtypeInstances = Set.insert tyCtor acc.tysWithNewtypeInstances }

      _ ->
        acc

    extractTypeCtor = case _ of
      TypeConstructor (QualifiedName tyCtor) -> Just tyCtor.name
      TypeApp (TypeConstructor (QualifiedName (tyCtor))) _ -> Just tyCtor.name
      TypeParens (Wrapped { value }) -> extractTypeCtor value
      _ -> Nothing

hasDecls :: Module Void -> Boolean
hasDecls (Module { body: ModuleBody { decls } }) = not $ Array.null decls

data DeclTypeKeyword
  = Newtype_WrappedType (Type Void)
  | Data_Constructors (NEA.NonEmptyArray (DataCtor Void))
  | Type_AliasedType (Type Void)

type DeclType =
  { tyName :: Proper
  , tyVars :: Array (TypeVarBinding Void)
  , keyword :: DeclTypeKeyword
  }

genOptic
  :: Partial
  => CliArgs
  -> GenOpticInfo
  -> DeclType
  -> Codegen Void GenOpticResult
genOptic opt otherInfo { tyName, tyVars, keyword } = case keyword of
  Type_AliasedType aliasedTy -> do
    unfoundRefTypes <-
      if opt.genTypeAliasLens then do
        identity_ <- importFrom "Prelude" $ importValue "identity"
        tyIso' <- importFrom "Data.Lens.Iso" $ importType "Iso'"
        void $ importFrom otherInfo.sourceFileModName $ importType $ unwrap tyName
        unfoundRefTypes <- genImportedType otherInfo aliasedTy
        let
          declIdentifier = "_" <> (unwrap tyName)
        tell
          [ declSignature declIdentifier
              $ typeForall (map dropTyVarKind tyVars)
              $ typeApp (typeCtor tyIso')
                  [ (typeApp (typeCtor tyName) $ map tyVarToTypeVar tyVars)
                  , aliasedTy
                  ]
          , declValue declIdentifier [] (exprIdent identity_)
          ]
        pure unfoundRefTypes
      else do
        pure $ Disj false
    pure
      { labelNames: extractReferencedLabelNames aliasedTy
      , unfoundRefTypes
      }

  Data_Constructors constructors -> do
    case NEA.uncons constructors of
      { head: DataCtor { name: ctorName , fields }, tail: [] } -> do
          void $ importFrom otherInfo.sourceFileModName $ importTypeAll $ unwrap tyName
          genLensProduct opt otherInfo tyName tyVars (unName ctorName) fields

      _ -> do
        void $ importFrom otherInfo.sourceFileModName $ importTypeAll (unwrap tyName)
        sets <- for constructors \(DataCtor { name: ctorName , fields }) ->
          genPrismSum opt otherInfo tyName tyVars (unName ctorName) fields
        pure $ foldl
          (\acc next -> { labelNames: Set.union acc.labelNames next.labelNames, unfoundRefTypes: acc.unfoundRefTypes <> next.unfoundRefTypes })
          { labelNames: Set.empty, unfoundRefTypes: Disj false }
          sets

  Newtype_WrappedType wrappedTy -> do
    tyLens' <- importFrom "Data.Lens" $ importType "Lens'"
    lensNewtype <- importFrom "Data.Lens.Iso.Newtype" $ importValue "_Newtype"
    void $ importFrom otherInfo.sourceFileModName $ importType $ unwrap tyName
    unfoundRefTypes <- genImportedType otherInfo wrappedTy
    let
      declIdentifier = "_" <> (unwrap tyName)
    tell
      [ declSignature declIdentifier
          $ typeForall (map dropTyVarKind tyVars)
          $ typeApp (typeCtor tyLens')
              [ (typeApp (typeCtor tyName) $ map tyVarToTypeVar tyVars)
              , wrappedTy
              ]
      , declValue declIdentifier [] (exprIdent lensNewtype)
      ]
    pure
      { labelNames: extractReferencedLabelNames wrappedTy
      , unfoundRefTypes
      }

extractReferencedLabelNames :: Type Void -> Set String
extractReferencedLabelNames ty = case ty of
  TypeRecord (Wrapped { value: Row { labels: Just (Separated { head, tail }) } }) -> do
    foldl (\acc next -> Set.insert (unLabel $ snd next) acc) (Set.singleton $ unLabel head) tail
  _ ->
    Set.empty

tyVarToTypeVar :: TypeVarBinding Void -> Type Void
tyVarToTypeVar = case _ of
  TypeVarName n -> typeVar n
  TypeVarKinded (Wrapped { value: Labeled { label: n } }) -> typeVar n

dropTyVarKind :: TypeVarBinding Void -> TypeVarBinding Void
dropTyVarKind = case _ of
  TypeVarKinded (Wrapped { value: Labeled { label: n } }) -> TypeVarName n
  n -> n

genImportedType
  :: Partial
  => GenOpticInfo
  -> Type Void
  -> Codegen Void (Disj Boolean)
genImportedType { referencedTypes } = go
  where
  foldDisj :: forall f. Foldable f => f (Disj Boolean) -> Disj Boolean
  foldDisj = foldl (<>) mempty

  go :: Type Void -> Codegen Void (Disj Boolean)
  go = case _ of
    TypeVar _ ->
      pure $ Disj false
    TypeConstructor tyCons ->
      importTypeConstructor tyCons

    TypeWildcard _ ->
      pure $ Disj false
    TypeHole _ ->
      pure $ Disj false
    TypeString _ _ ->
      pure $ Disj false
    TypeRow (Wrapped { value }) ->
      goRow value
    TypeRecord (Wrapped { value }) ->
      goRow value
    TypeForall _ tyVars _ ty -> ado
      a <- goTyVars tyVars
      b <- go ty
      in a <> b
    TypeKinded ty _ kind -> ado
      a <- go ty
      b <- go kind
      in a <> b
    TypeApp f as -> ado
      a <- go f
      bs <- map foldDisj $ traverse go as
      in a <> bs
    TypeOp l ops -> ado
      a <- go l
      bs <- map foldDisj $ for ops \(Tuple qualNameOp ty) -> ado
        x <- importTypeOperator qualNameOp
        y <- go ty
        in x <> y
      in a <> bs
    TypeOpName qualNameOp ->
      importTypeOperator qualNameOp
    TypeArrow f _ a -> ado
      x <- go f
      y <- go a
      in x <> y
    TypeArrowName _ ->
      pure $ Disj false
    TypeConstrained constraint _ ty -> ado
      a <- go constraint
      b <- go ty
      in a <> b
    TypeParens (Wrapped { value }) ->
      go value
    TypeUnaryRow _ ty ->
      go ty
    TypeError a ->
      absurd a

  goRow (Row r) = ado
    lbls <- r.labels # maybe (pure (Disj false)) \sep -> do
      map fold $ for (unSeparated sep) \(Labeled { value }) -> do
        go value
    tail <- r.tail # maybe (pure (Disj false)) \(Tuple _ ty) -> go ty
    in lbls <> tail

  goTyVars :: NonEmptyArray (TypeVarBinding Void) -> Codegen Void (Disj Boolean)
  goTyVars = map foldDisj <<< traverse case _ of
    TypeVarName _ -> pure $ Disj false
    TypeVarKinded (Wrapped { value: Labeled { value } }) ->
      go value

  importTypeConstructor :: QualifiedName Proper -> Codegen Void (Disj Boolean)
  importTypeConstructor (QualifiedName r) = do
    let
      tyName :: Proper
      tyName = r.name

      byTypeName = Map.lookup (TypeName r.module tyName) referencedTypes
      byModAlias = r.module >>= \modName -> Map.lookup (ModuleAlias modName) referencedTypes
      qualifiedTyName = maybe "" (flip append "." <<< unwrap) r.module <> unwrap tyName
    case byTypeName <|> byModAlias of
      Just modName -> do
        void $ importFrom modName $ importType qualifiedTyName
        pure $ Disj false

      Nothing ->
        pure $ Disj true

  importTypeOperator :: QualifiedName Operator -> Codegen Void (Disj Boolean)
  importTypeOperator (QualifiedName r) = do
    let
      opName :: Operator
      opName = r.name

      byOpName = Map.lookup (TypeOperator r.module opName) referencedTypes
      byModAlias = r.module >>= \modName -> Map.lookup (ModuleAlias modName) referencedTypes
      qualifiedOpName = maybe "" (flip append "." <<< unwrap) r.module <> unwrap opName
    case byOpName <|> byModAlias of
      Just modName -> do
        void $ importFrom modName $ importTypeOp qualifiedOpName
        pure $ Disj false

      Nothing ->
        pure $ Disj true

genLensProduct
  :: Partial
  => CliArgs
  -> GenOpticInfo
  -> Proper
  -> Array (TypeVarBinding Void)
  -> Proper
  -> Array (Type Void)
  -> Codegen Void GenOpticResult
genLensProduct opt otherInfo tyName tyVars ctorName fields = do
  tyLens' <- importFrom "Data.Lens" $ importType "Lens'"
  iso <- importFrom "Data.Lens.Iso" $ importValue "iso"
  unfoundRefTypes <- map fold $ traverse (genImportedType otherInfo) fields
  let
    declIdentifier = "_" <> (unwrap ctorName)
    returning labelNames = pure { labelNames, unfoundRefTypes }
  case fields of
    -- Given:
    --    data Foo a b c = Foo
    -- Produce
    --    _Foo :: forall a b c. Lens' (Foo a b c) Unit
    --    _Foo = lens' (const unit) (const Foo)
    [] -> do
      prelude <- importFrom "Prelude"
        { const_: importValue "const"
        , unit_: importValue "unit"
        , unitType: importType "Unit"
        }
      tell
        [ declSignature declIdentifier
            $ typeForall (map dropTyVarKind tyVars)
            $ typeApp (typeCtor tyLens')
                [ typeApp (typeCtor tyName) $ map tyVarToTypeVar tyVars
                , typeCtor prelude.unitType
                ]
        , declValue declIdentifier [] do
            exprApp (exprIdent iso)
              [ exprApp (exprIdent prelude.const_) [ exprIdent prelude.unit_ ]
              , exprApp (exprIdent prelude.const_) [ exprCtor ctorName ]
              ]
        ]
      returning Set.empty
    -- Given:
    --    data Foo a b c = Foo a
    -- Produce
    --    _Foo :: forall a b c. Lens' (Foo a b c) Unit
    --    _Foo = lens' (\(Foo a) -> a) Foo
    [ ty1 ] -> do
      tell
        [ declSignature declIdentifier
            $ typeForall (map dropTyVarKind tyVars)
            $ typeApp (typeCtor tyLens')
                [ typeApp (typeCtor tyName) $ map tyVarToTypeVar tyVars
                , ty1
                ]
        , declValue declIdentifier [] do
            exprApp (exprIdent iso)
              [ exprLambda [ binderCtor ctorName [ binderVar "a" ] ] (exprIdent "a")
              , exprCtor ctorName
              ]
        ]
      returning $ extractReferencedLabelNames ty1

    -- Given:
    --    data Foo a b c = Foo a b
    -- Produce
    --    _Foo :: forall a b c. Lens' (Foo a b c) (Tuple a b)
    --    _Foo = lens' (\(Foo a b) -> Tuple a b) (\(Tuple a b) -> Foo a b)
    [ ty1, ty2 ] -> do
      tupleRec <- importFrom "Data.Tuple"
        { ty: importType "Tuple"
        , ctor: importCtor "Tuple" "Tuple"
        }
      tell
        [ declSignature declIdentifier
            $ typeForall (map dropTyVarKind tyVars)
            $ typeApp (typeCtor tyLens')
                [ typeApp (typeCtor tyName) $ map tyVarToTypeVar tyVars
                , typeApp (typeCtor tupleRec.ty) [ ty1, ty2 ]
                ]
        , declValue declIdentifier [] do
            exprApp (exprIdent iso)
              [ exprLambda [ binderCtor ctorName [ binderVar "a", binderVar "b" ] ]
                  (exprApp (exprCtor tupleRec.ctor) [ exprIdent "a", exprIdent "b" ])
              , exprLambda [ binderCtor tupleRec.ctor [ binderVar "a", binderVar "b" ] ]
                  (exprApp (exprCtor ctorName) [ exprIdent "a", exprIdent "b" ])
              ]
        ]
      returning $ Set.union (extractReferencedLabelNames ty1) (extractReferencedLabelNames ty2)
    -- Given:
    --    data Foo a b ... n = Foo a b ... n
    -- Produce
    --    _Foo
    --      :: forall a b ... n
    --       . Lens'
    --           (Foo a b ... n)
    --           { arg1 :: a, arg2 :: b, ..., argN :: n }
    --    _Foo = lens'
    --      (\(Foo a b ... n) -> { arg1: a, arg2: b, ..., argN: n })
    --      \r -> Foo r.arg1 r.arg2 ... r.argN
    _ -> do
      let
        { recordTy, varNames } = fieldLabels fields opt.recordLabelStyle
      tell
        [ declSignature declIdentifier
            $ typeForall (map dropTyVarKind tyVars)
            $ typeApp (typeCtor tyLens')
                [ typeApp (typeCtor tyName) $ map tyVarToTypeVar tyVars
                , typeRecord recordTy Nothing
                ]
        , declValue declIdentifier [] do
            exprApp (exprIdent iso)
              [ exprLambda [ binderCtor ctorName $ map binderVar varNames ]
                  (exprRecord $ map (\var -> Tuple var (exprIdent var)) varNames)
              , exprLambda [ binderRecord varNames ]
                  (exprApp (exprCtor ctorName) $ map exprIdent varNames)
              ]
        ]
      returning $ Set.fromFoldable varNames

genPrismSum
  :: Partial
  => CliArgs
  -> GenOpticInfo
  -> Proper
  -> Array (TypeVarBinding Void)
  -> Proper
  -> Array (Type Void)
  -> Codegen Void GenOpticResult
genPrismSum opt otherInfo tyName tyVars ctorName fields = do
  tyPrism' <- importFrom "Data.Lens.Prism" $ importType "Prism'"
  prismFn <- importFrom "Data.Lens.Prism" $ importValue "prism"
  eitherRec <- importFrom "Data.Either"
    { leftCtor: importCtor "Either" "Left"
    , rightCtor: importCtor "Either" "Right"
    , ty: importType "Either"
    }
  unfoundRefTypes <- map fold $ traverse (genImportedType otherInfo) fields
  let
    declIdentifier = "_" <> (unwrap ctorName)
    returning labelNames = pure { labelNames, unfoundRefTypes }
  case fields of
    -- Given:
    --    data Foo a b c
    --      = Bar
    --      | ... other constructors
    -- Produce
    --    _Bar :: forall a b c. Prism' (Foo a b c) Unit
    --    _Bar = prism' (const Bar) case _ of
    --      Bar -> Right unit
    --      x -> Left x
    [] -> do
      prelude <- importFrom "Prelude"
        { const_: importValue "const"
        , unit_: importValue "unit"
        , unitType: importType "Unit"
        }
      tell
        [ declSignature declIdentifier
            $ typeForall (map dropTyVarKind tyVars)
            $ typeApp (typeCtor tyPrism')
                [ typeApp (typeCtor tyName) $ map tyVarToTypeVar tyVars
                , typeCtor prelude.unitType
                ]
        , declValue declIdentifier [] do
            exprApp (exprIdent prismFn)
              [ exprApp (exprIdent prelude.const_) [ exprCtor ctorName ]
              , exprCase [ exprSection ]
                  [ caseBranch [ binderCtor ctorName [] ]
                      (exprApp (exprCtor eitherRec.rightCtor) [ exprIdent prelude.unit_ ])
                  , caseBranch [ binderVar "other" ]
                      (exprApp (exprCtor eitherRec.leftCtor) [ exprIdent "other" ])
                  ]
              ]
        ]
      returning Set.empty
    -- Given:
    --    data Foo a b c
    --      = Bar a
    --      | ... other constructors
    -- Produce
    --    _Bar :: forall a b c. Prism' (Foo a b c) a
    --    _Bar = prism' Bar case _ of
    --      Bar a -> Right a
    --      x -> Left x
    [ ty1 ] -> do
      tell
        [ declSignature declIdentifier
            $ typeForall (map dropTyVarKind tyVars)
            $ typeApp (typeCtor tyPrism')
                [ typeApp (typeCtor tyName) $ map tyVarToTypeVar tyVars
                , ty1
                ]
        , declValue declIdentifier [] do
            exprApp (exprIdent prismFn)
              [ exprCtor ctorName
              , exprCase [ exprSection ]
                  [ caseBranch [ binderCtor ctorName [ binderVar "a" ] ]
                      (exprApp (exprCtor eitherRec.rightCtor) [ exprIdent "a" ])
                  , caseBranch [ binderVar "other" ]
                      (exprApp (exprCtor eitherRec.leftCtor) [ exprIdent "other" ])
                  ]
              ]
        ]
      returning $ extractReferencedLabelNames ty1

    -- Given:
    --    data Foo a b c
    --      = Bar a b
    --      | ... other constructors
    -- Produce
    --    _Bar :: forall a b c. Prism' (Foo a b c) (Tuple a b)
    --    _Bar = prism' Bar case _ of
    --      Bar a b -> Right (Tuple a b)
    --      x -> Left x
    [ ty1, ty2 ] -> do
      tupleRec <- importFrom "Data.Tuple"
        { ty: importType "Tuple"
        , ctor: importCtor "Tuple" "Tuple"
        }
      tell
        [ declSignature declIdentifier
            $ typeForall (map dropTyVarKind tyVars)
            $ typeApp (typeCtor tyPrism')
                [ typeApp (typeCtor tyName) $ map tyVarToTypeVar tyVars
                , typeApp (typeCtor tupleRec.ty) [ ty1, ty2 ]
                ]
        , declValue declIdentifier [] do
            exprApp (exprIdent prismFn)
              [ exprLambda [ binderCtor tupleRec.ctor [ binderVar "a", binderVar "b" ] ]
                  (exprApp (exprCtor ctorName) [ exprIdent "a", exprIdent "b" ])
              , exprCase [ exprSection ]
                  [ caseBranch [ binderCtor ctorName [ binderVar "a", binderVar "b" ] ]
                      (exprApp (exprCtor eitherRec.rightCtor) [ exprApp (exprCtor tupleRec.ctor) [ exprIdent "a", exprIdent "b" ] ])
                  , caseBranch [ binderVar "other" ]
                      (exprApp (exprCtor eitherRec.leftCtor) [ exprIdent "other" ])
                  ]
              ]
        ]
      returning $ Set.union (extractReferencedLabelNames ty1) (extractReferencedLabelNames ty2)

    -- Given:
    --    data Foo a b ... n
    --      = Bar a b ... n
    --      | ... other constructors
    -- Produce
    --    _Bar
    --      :: forall a b ... n
    --       . Prism'
    --           (Foo a b ... n)
    --           { arg1 :: a, arg2 :: b, ..., argN :: n }
    --    _Bar = prism'
    --      (\r -> Bar r.arg1 r.arg2 ... r.argN)
    --      case _ of
    --        Bar a b ... n -> Right { arg1: a, arg2: b, ..., argN: n }
    --        x -> Left x
    _ -> do
      let
        { recordTy, varNames } = fieldLabels fields opt.recordLabelStyle
      tell
        [ declSignature declIdentifier
            $ typeForall (map dropTyVarKind tyVars)
            $ typeApp (typeCtor tyPrism')
                [ typeApp (typeCtor tyName) $ map tyVarToTypeVar tyVars
                , typeRecord recordTy Nothing
                ]
        , declValue declIdentifier [] do
            exprApp (exprIdent prismFn)
              [ exprLambda [ binderRecord varNames ]
                  (exprApp (exprCtor ctorName) $ map exprIdent varNames)
              , exprCase [ exprSection ]
                  [ caseBranch [ binderCtor ctorName $ map binderVar varNames ]
                      ( exprApp (exprCtor eitherRec.rightCtor)
                          [ exprRecord $ map (\var -> Tuple var (exprIdent var)) varNames ]
                      )
                  , caseBranch [ binderVar "other" ]
                      (exprApp (exprCtor eitherRec.leftCtor) [ exprIdent "other" ])
                  ]
              ]
        ]
      returning $ Set.fromFoldable varNames

-- | Given `genLensProp ["label"]`, produces the following:
-- | ```
-- | _propLabel :: forall r a. Lens' { label :: a | r } a
-- | _propLabel = prop (Proxy :: Proxy "label")
-- | ```
genLensProp :: Partial => Maybe NonEmptyString -> Set String -> Codegen Void Unit
genLensProp lblPrefix labelsInFile = do
  tyLens' <- importFrom "Data.Lens" $ importType "Lens'"
  prop <- importFrom "Data.Lens.Record" $ importValue "prop"
  proxy <- importFrom "Type.Proxy"
    { ctor: importCtor "Proxy" "Proxy"
    , ty: importType "Proxy"
    }
  for_ labelsInFile \label -> do
    let
      declIdentifier = case lblPrefix of
        Nothing -> "_" <> label
        Just prefix -> "_" <> NES.toString prefix <> uppercaseFirstChar label
    tell
      [ declSignature declIdentifier
          $ typeForall [ typeVar "r", typeVar "a" ]
          $ typeApp (typeCtor tyLens')
              [ typeRecord
                  [ Tuple label (typeVar "a") ]
                  (Just (typeVar "r"))
              , typeVar "a"
              ]
      , declValue declIdentifier [] do
          exprApp (exprIdent prop)
            [ exprTyped (exprCtor proxy.ctor) (typeApp (typeCtor proxy.ty) [ (typeString label) ]) ]
      ]

uppercaseFirstChar :: String -> String
uppercaseFirstChar = splitAt 1 >>> (\r -> (toUpper r.before) <> r.after)

fieldLabels
  :: Partial
  => Array (Type Void)
  -> RecordLabelStyle
  -> { recordTy :: Array (Tuple String (Type Void)), varNames :: Array String }
fieldLabels fields = case _ of
  ArgRecordLabels ->
    { recordTy: mapWithIndex (\i ty -> Tuple ("arg" <> show (i + 1)) ty) fields
    , varNames: mapWithIndex (\i _ -> "arg" <> show (i + 1)) fields
    }
  AlphabetRecordLabels -> do
    foldlFn \acc ty -> case acc.init, acc.nextLabel of
      true, _ ->
        acc
          { nextLabel = acc.nextLabel + 1
          , init = false
          , recordTy = Array.snoc acc.recordTy $ Tuple (charCodeToString acc.nextLabel) ty
          , varNames = Array.snoc acc.varNames $ charCodeToString acc.nextLabel
          }
      false, x
        | x == charCodeZ ->
            acc
              { nextLabel = charCodeA
              , prefix = maybe (Just 0) (\i -> Just $ i + 1) acc.prefix
              , recordTy = Array.snoc acc.recordTy $ Tuple (nextVar acc.prefix acc.nextLabel) ty
              , varNames = Array.snoc acc.varNames $ nextVar acc.prefix acc.nextLabel
              }
        | otherwise ->
            acc
              { nextLabel = acc.nextLabel + 1
              , recordTy = Array.snoc acc.recordTy $ Tuple (nextVar acc.prefix acc.nextLabel) ty
              , varNames = Array.snoc acc.varNames $ nextVar acc.prefix acc.nextLabel
              }
    where
    charCodeA = toCharCode 'a' -- 97
    charCodeZ = toCharCode 'z' -- 122
    initial =
      { nextLabel: charCodeA
      , prefix: (Nothing :: Maybe Int)
      , init: false
      , recordTy: []
      , varNames: []
      }
    charCodeToString = fromJust <<< map String.singleton <<< fromCharCode
    nextVar prefix nextLabel =
      (maybe "" (\i -> charCodeToString (charCodeA + i)) prefix) <> charCodeToString nextLabel

    foldlFn
      :: ( { nextLabel :: Int
           , prefix :: Maybe Int
           , init :: Boolean
           , recordTy :: Array (Tuple String (Type Void))
           , varNames :: Array String
           }
           -> Type Void
           -> { nextLabel :: Int
              , prefix :: Maybe Int
              , init :: Boolean
              , recordTy :: Array (Tuple String (Type Void))
              , varNames :: Array String
              }
         )
      -> { recordTy :: Array (Tuple String (Type Void)), varNames :: Array String }
    foldlFn f = do
      let { recordTy, varNames } = foldl f initial fields
      { recordTy, varNames }

unWrappedSeparated :: forall a. Wrapped (Separated a) -> Array a
unWrappedSeparated (Wrapped { value }) = unSeparated value

unSeparated :: forall a. Separated a -> Array a
unSeparated (Separated { head, tail }) = Array.cons head $ map snd tail

unLabel :: forall a. Labeled (Name Label) a -> String
unLabel (Labeled { label: Name { name: Label label } }) = label

unName :: forall a. Name a -> a
unName (Name { name }) = name
