module MkLens where

import Prelude
import Prim hiding (Type, Row)

import Control.Alt ((<|>))
import Control.Monad.Free (runFree)
import Control.Monad.Writer (tell)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Char (fromCharCode, toCharCode)
import Data.Identity (Identity(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String (splitAt, toUpper)
import Data.String.CodeUnits as String
import Data.Traversable (foldl, for, for_, traverse, traverse_)
import Data.Tuple (Tuple(..), snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSA
import Node.Path (FilePath, basenameWithoutExt, dirname, extname)
import Node.Path as Path
import Partial.Unsafe (unsafePartial)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types (DataCtor(..), DataMembers(..), Declaration(..), Export(..), FixityOp(..), Foreign(..), Import(..), ImportDecl(..), Label(..), Labeled(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName, Name(..), Operator, Proper(..), QualifiedName(..), Row(..), Separated(..), Type(..), TypeVarBinding(..), Wrapped(..))
import Safe.Coerce (coerce)
import Tidy.Codegen (binderCtor, binderRecord, binderVar, caseBranch, declSignature, declValue, exprApp, exprCase, exprCtor, exprIdent, exprLambda, exprRecord, exprSection, exprTyped, printModule, typeApp, typeCtor, typeForall, typeRecord, typeString, typeVar)
import Tidy.Codegen.Monad (Codegen, importClass, importCtor, importFrom, importOp, importOpen, importOpenHiding, importType, importTypeAll, importTypeOp, importValue, runCodegenTModule)
import Types (RecordLabelStyle(..))

type GenOptions =
  { genTypeAliasLens :: Boolean
  , genGlobalPropFile :: Maybe { filePath :: FilePath, moduleName :: String, overwrite :: Boolean }
  , recordLabelStyle :: RecordLabelStyle
  }

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

-- | Uses a two-step pass to generate all lenses/prisms for a file
-- | 1. Generate a lens/prism for a data type and newtype
-- | 2. Generate a lens for each label referenced in the previous generated
-- |    lenses or prisms.
generateLensModule :: GenOptions -> String -> Aff (Set String)
generateLensModule options filePath = do
  content <- FSA.readTextFile UTF8 filePath
  case parseModule content of
    ParseSucceeded cst -> do
      let
        sourceFileModName = getSourceFileModuleName cst
        modulePath = (unwrap sourceFileModName) <> ".Lens"
        Tuple labelNames generatedModule = runFree coerce $ unsafePartial $ runCodegenTModule modulePath do
          importOpenImports cst
          labelNames <- traverse (genOptic options sourceFileModName (getExportMap cst) (getImportedTypes cst) (getTypesWithDerivedNewtypeInstances cst)) $ extractDecls cst
          let labelNameSet = foldl Set.union Set.empty labelNames
          unless (isJust options.genGlobalPropFile) do
            genLensProp labelNameSet
          pure labelNameSet
      when (hasDecls generatedModule) do
        let
          outputtedContent = printModule generatedModule
          parentDir = Path.concat [ dirname filePath, basenameWithoutExt filePath (extname filePath) ]
        unlessM (FSA.exists parentDir) do
          FSA.mkdir parentDir
        FSA.writeTextFile UTF8 (Path.concat [ parentDir, "Lens.purs" ]) outputtedContent
      pure labelNames
    _ ->
      liftEffect $ throw $
        "Parsing module for file path failed. Could not generate lens file for path: '" <> filePath <> "'"
  where
  getSourceFileModuleName (Module { header: ModuleHeader { name: Name { name } } }) = name

  getExportMap :: Module Void -> Maybe ExportedTypeMap
  getExportMap (Module { header: ModuleHeader { exports }}) = map (foldl foldFn Map.empty <<< unWrappedSeparated) exports
    where
    foldFn :: ExportedTypeMap -> Export Void -> ExportedTypeMap
    foldFn acc = case _ of
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

  importOpenImports :: Partial => Module Void -> Codegen Void Unit
  importOpenImports (Module { header: ModuleHeader { imports }}) =
    for_ imports case _ of
      ImportDecl r@{ names: Nothing, qualified: Nothing } -> do
        importOpen $ unName r.module
      ImportDecl r@{ names: Just (Tuple (Just _hidingKeyword) _members), qualified: q } -> do
        for_ (unWrappedSeparated _members) case _ of
          ImportValue impName ->
            importOpenHiding (unName r.module) $ importValue $ qualify q $ unwrap $ unName impName
          ImportOp opName -> do
            importOpenHiding (unName r.module) $ importOp $ qualify q $ unwrap $ unName opName
          ImportType tyName Nothing ->
            importOpenHiding (unName r.module) $ importType $ qualify q $ unwrap $ unName tyName
          ImportType tyName (Just (DataAll _)) -> do
            importOpenHiding (unName r.module) $ importTypeAll $ qualify q $ unwrap $ unName tyName
          ImportType tyName (Just (DataEnumerated (Wrapped { value }))) ->
            case value of
              Nothing ->
                -- I believe this is...
                --    import Foo hiding ()
                -- which gets me a parser error when I tried it on Try PureScript
                -- So, we'll work around it by just importing it open
                -- since nothing is being hidden.
                importOpen (unName r.module)
              Just sep ->
                for_ (unSeparated sep) \nameProper ->
                  importOpenHiding (unName r.module)
                    $ importCtor (unwrap $ unName tyName)
                    $ qualify q $ unwrap $ unName nameProper
          ImportTypeOp _ opName ->
            importOpenHiding (unName r.module) $ importTypeOp $ qualify q $ unwrap $ unName opName
          ImportClass _ className ->
            importOpenHiding (unName r.module) $ importClass $ qualify q $ unwrap $ unName className
          ImportKind _ _ ->
            -- kinds cannot be imported, and this will be deprecated anyways
            pure unit
          ImportError e ->
            absurd e
        where
          -- The only way to force tidy-codegen to import declarations in a qualified matter
          -- is to use `Qualifier.import`
          qualify qualified s =
            (maybe "" (flip append "." <<< unwrap <<< unName) $ map snd qualified) <> s
      _ -> pure unit

  getImportedTypes :: Module Void -> ImportedTypes
  getImportedTypes
    ( Module
        { header: ModuleHeader { imports, name: Name { name: sourceFileModName } }
        , body: ModuleBody { decls }
        }
    ) = foldl insertTypesDefinedInSourceFile typesImportedBySourceFile decls
    where
    typesImportedBySourceFile = foldl insertImportedTypes Map.empty imports

    insertTypesDefinedInSourceFile :: ImportedTypes -> Declaration Void -> ImportedTypes
    insertTypesDefinedInSourceFile acc = case _ of
      DeclData { name } _ -> do
        Map.insert (TypeName Nothing (unName name)) sourceFileModName acc
      DeclNewtype { name } _ _ _ -> do
        Map.insert (TypeName Nothing (unName name)) sourceFileModName acc
      DeclType { name } _ _ -> do
        Map.insert (TypeName Nothing (unName name)) sourceFileModName acc
      DeclForeign _ _ (ForeignData _ (Labeled { label })) -> do
        Map.insert (TypeName Nothing (unName label)) sourceFileModName acc
      DeclFixity { operator: FixityType _ _ _ opName } -> do
        Map.insert (TypeOperator Nothing (unName opName)) sourceFileModName acc
      _ ->
        acc

    insertImportedTypes :: ImportedTypes -> ImportDecl Void -> ImportedTypes
    insertImportedTypes acc (ImportDecl r) = do
      case map (snd >>> unName) r.qualified, r.names of
        -- Open import: import with no module alias or imported members:
        --   import Prelude
        -- Open imports are imported into the generated module via `importOpenImports`
        Nothing, Nothing -> acc

        -- Open import with a few members hidden.
        --   import Prelude hiding (show)
        -- Open imports are imported into the generated module via `importOpenImports`
        Nothing, Just (Tuple (Just _hidingKeyword) _) -> acc

        -- Import with only a module alias. We might be using types from it via its alias:
        --   import Foo as Q
        Just aliasModName, Nothing -> do
          Map.insert (ModuleAlias aliasModName) (unName r.module) acc

        -- Import with module alias, hiding a few types.
        --   import Bar hiding (MyOtherType) as Q
        Just aliasModName, Just (Tuple (Just _hidingKeyword) _) -> do
          Map.insert (ModuleAlias aliasModName) (unName r.module) acc

        -- Imported members that might include a module alias.
        --   import Foo (MyType)
        --   import Bar (MyOtherType) as Q
        possibleModAlias, Just (Tuple Nothing members) -> do
          foldl insertType acc $ unWrappedSeparated members
          where
          insertType accum = case _ of
            ImportType tyNameProper _ -> Map.insert (TypeName possibleModAlias (unName tyNameProper)) (unName r.module) accum
            ImportTypeOp _ opName -> Map.insert (TypeOperator possibleModAlias (unName opName)) (unName r.module) accum
            _ -> acc

  getTypesWithDerivedNewtypeInstances :: Module Void -> Set Proper
  getTypesWithDerivedNewtypeInstances (Module { body: ModuleBody { decls } }) =
    foldl foldFn Set.empty decls
    where
    foldFn :: Set Proper -> Declaration Void -> Set Proper
    foldFn acc = case _ of
      -- derive instance Newtype TyNameNoTyVars _
      -- derive instance Newtype (TyNameWithTyVars a b c) _
      DeclDerive _ _ { className: QualifiedName r, types: [ tyCtorWithPossibleArgs, _underscore ] }
        | (unwrap $ r.name) == "Newtype" -- note: what if imported qualified? need to figure out what module alias is
        , Just tyCtor <- extractTypeCtor tyCtorWithPossibleArgs ->
            Set.insert tyCtor acc

      _ ->
        acc

    extractTypeCtor = case _ of
      TypeConstructor (QualifiedName tyCtor) -> Just tyCtor.name
      TypeApp (TypeConstructor (QualifiedName (tyCtor))) _ -> Just tyCtor.name
      _ -> Nothing

hasDecls :: Module Void -> Boolean
hasDecls (Module { body: ModuleBody { decls } }) = not $ Array.null decls

data DeclTypeKeyword
  = Newtype_WrappedType (Type Void)
  | Data_Constructors (Array (DataCtor Void))
  | Type_AliasedType (Type Void)

type DeclType =
  { tyName :: Name Proper
  , tyVars :: Array (TypeVarBinding Void)
  , keyword :: DeclTypeKeyword
  }

extractDecls :: Module Void -> Array DeclType
extractDecls (Module { body: ModuleBody { decls } }) = foldl foldFn [] decls
  where
  foldFn acc = case _ of
    DeclData ({ name, vars }) (Just (Tuple _ sep)) -> do
      Array.snoc acc $ { tyName: name, tyVars: vars, keyword: Data_Constructors (unSeparated sep) }
    DeclNewtype ({ name, vars }) _ _ ty -> do
      Array.snoc acc $ { tyName: name, tyVars: vars, keyword: Newtype_WrappedType ty }
    DeclType ({ name, vars }) _ ty -> do
      Array.snoc acc $ { tyName: name, tyVars: vars, keyword: Type_AliasedType ty }
    _ -> acc

genOptic
  :: Partial
  => GenOptions
  -> ModuleName
  -> Maybe ExportedTypeMap
  -> ImportedTypes
  -> Set Proper
  -> DeclType
  -> Codegen Void (Set String)
genOptic opt souceFileModName exportMap importMap typesWithDeriveNewtypeInstance { tyName, tyVars, keyword } =
  case exportMap of
    -- Everything was exported
    --    module Foo where
    Nothing -> case keyword of
      Type_AliasedType aliasedTy -> genTypeAliasLens aliasedTy
      Data_Constructors constructors -> genDataOptic constructors
      Newtype_WrappedType wrappedTy
        | Set.member (unName tyName) typesWithDeriveNewtypeInstance -> genNewtypeOptic wrappedTy
        | otherwise -> pure Set.empty

    -- Only some things were exported
    --    module Foo (something) where
    Just exportedTys -> case Map.lookup (unName tyName) exportedTys of
      -- Type wasn't exported
      Nothing ->
        pure Set.empty

      -- Type was exported, but none of its members (if it has any)
      Just DCMNone -> case keyword of
        Data_Constructors _ -> pure Set.empty
        Newtype_WrappedType _ -> pure Set.empty
        Type_AliasedType aliasedTy -> genTypeAliasLens aliasedTy

      Just DCMAll -> case keyword of
        Type_AliasedType _ -> pure Set.empty
        Data_Constructors constructors -> genDataOptic constructors
        Newtype_WrappedType wrappedTy
          | Set.member (unName tyName) typesWithDeriveNewtypeInstance -> genNewtypeOptic wrappedTy
          | otherwise -> pure Set.empty
  where
    genDataOptic constructors = case constructors of
      -- This can never occur because `DeclData` case above only matches on `Just`
      -- which means there is at least one data constructor.
      [] ->
        pure Set.empty

      [ DataCtor { name: ctorName@(Name { name: (Proper ctorNameStr) }), fields } ] -> do
        void $ importFrom souceFileModName $ importTypeAll $ unwrap $ unName tyName
        genLensProduct opt importMap tyName ctorName tyVars ctorNameStr fields

      _ -> do
        void $ importFrom souceFileModName $ importTypeAll (unwrap $ unName tyName)
        sets <- for constructors \(DataCtor { name: ctorName@(Name { name: Proper ctorNameStr }), fields }) ->
          genPrismSum opt importMap tyName ctorName tyVars ctorNameStr fields
        pure $ foldl Set.union Set.empty sets

    genNewtypeOptic wrappedTy = do
      tyLens' <- importFrom "Data.Lens" $ importType "Lens'"
      lensNewtype <- importFrom "Data.Lens.Iso.Newtype" $ importValue "_Newtype"
      void $ importFrom souceFileModName $ importType $ unwrap $ unName tyName
      genImportedType importMap wrappedTy
      let
        declIdentifier = "_" <> (unwrap $ unName tyName)
      tell
        [ declSignature declIdentifier
            $ typeForall tyVars
            $ typeApp (typeCtor tyLens')
                [ (typeApp (typeCtor tyName) $ map tyVarToTypeVar tyVars)
                , wrappedTy
                ]
        , declValue declIdentifier [] (exprIdent lensNewtype)
        ]
      pure $ extractReferencedLabelNames wrappedTy

    genTypeAliasLens aliasedTy = do
      when opt.genTypeAliasLens do
        identity_ <- importFrom "Prelude" $ importValue "identity"
        tyLens' <- importFrom "Data.Lens" $ importType "Lens'"
        void $ importFrom souceFileModName $ importType $ unwrap $ unName tyName
        genImportedType importMap aliasedTy
        let
          declIdentifier = "_" <> (unwrap $ unName tyName)
        tell
          [ declSignature declIdentifier
              $ typeForall tyVars
              $ typeApp (typeCtor tyLens')
                  [ (typeApp (typeCtor tyName) $ map tyVarToTypeVar tyVars)
                  , aliasedTy
                  ]
          , declValue declIdentifier [] (exprIdent identity_)
          ]
      pure $ extractReferencedLabelNames aliasedTy

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

genImportedType
  :: Partial
  => ImportedTypes
  -> Type Void
  -> Codegen Void Unit
genImportedType importMap = go
  where
  go = case _ of
    TypeVar _ -> pure unit
    TypeConstructor (QualifiedName r) -> do
      let
        tyName :: Proper
        tyName = r.name

        byTypeName = Map.lookup (TypeName r.module tyName) importMap
        byModAlias = r.module >>= \modName -> Map.lookup (ModuleAlias modName) importMap
        qualifiedTyName = maybe "" (flip append "." <<< unwrap) r.module <> unwrap tyName
      case byTypeName <|> byModAlias of
        Just modName -> do
          void $ importFrom modName $ importType qualifiedTyName
        Nothing ->
          pure unit

    TypeWildcard _ -> pure unit
    TypeHole _ -> pure unit
    TypeString _ _ -> pure unit
    TypeRow (Wrapped { value }) -> goRow value
    TypeRecord (Wrapped { value }) -> goRow value
    TypeForall _ tyVars _ ty -> goTyVars tyVars *> go ty
    TypeKinded ty _ kind -> go ty *> go kind
    TypeApp f as -> go f *> traverse_ go as
    TypeOp l ops -> go l *> for_ ops \(Tuple qualNameOp ty) -> do
      importTypeOperator qualNameOp *> go ty
    TypeOpName qualNameOp -> importTypeOperator qualNameOp
    TypeArrow f _ a -> go f *> go a
    TypeArrowName _ -> pure unit
    TypeConstrained constraint _ ty -> go constraint *> go ty
    TypeParens (Wrapped { value }) -> go value
    TypeUnaryRow _ ty -> go ty
    TypeError a -> absurd a

  goRow (Row r) = do
    for_ r.labels \sep -> do
      for_ (unSeparated sep) \(Labeled { value }) -> do
        go value
    for_ r.tail \(Tuple _ ty) -> go ty

  goTyVars = traverse_ case _ of
    TypeVarName _ -> pure unit
    TypeVarKinded (Wrapped { value: Labeled { value } }) -> go value

  importTypeOperator (QualifiedName r) = do
    let
      opName :: Operator
      opName = r.name

      byOpName = Map.lookup (TypeOperator r.module opName) importMap
      byModAlias = r.module >>= \modName -> Map.lookup (ModuleAlias modName) importMap
      qualifiedOpName = maybe "" (flip append "." <<< unwrap) r.module <> unwrap opName
    case byOpName <|> byModAlias of
      Just modName -> do
        void $ importFrom modName $ importTypeOp qualifiedOpName
      Nothing ->
        pure unit

genLensProduct
  :: Partial
  => GenOptions
  -> ImportedTypes
  -> Name Proper
  -> Name Proper
  -> Array (TypeVarBinding Void)
  -> String
  -> Array (Type Void)
  -> Codegen Void (Set String)
genLensProduct opt importMap tyName ctorName tyVars ctorNameStr fields = do
  tyLens' <- importFrom "Data.Lens" $ importType "Lens'"
  iso <- importFrom "Data.Lens.Iso" $ importValue "iso"
  traverse_ (genImportedType importMap) fields
  let
    declIdentifier = "_" <> ctorNameStr
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
            $ typeForall tyVars
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
      pure Set.empty
    -- Given:
    --    data Foo a b c = Foo a
    -- Produce
    --    _Foo :: forall a b c. Lens' (Foo a b c) Unit
    --    _Foo = lens' (\(Foo a) -> a) Foo
    [ ty1 ] -> do
      tell
        [ declSignature declIdentifier
            $ typeForall tyVars
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
      pure $ extractReferencedLabelNames ty1

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
            $ typeForall tyVars
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
      pure $ Set.union (extractReferencedLabelNames ty1) (extractReferencedLabelNames ty2)
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
            $ typeForall tyVars
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
      pure $ Set.fromFoldable varNames

genPrismSum
  :: Partial
  => GenOptions
  -> ImportedTypes
  -> Name Proper
  -> Name Proper
  -> Array (TypeVarBinding Void)
  -> String
  -> Array (Type Void)
  -> Codegen Void (Set String)
genPrismSum opt importMap tyName ctorName tyVars ctorNameStr fields = do
  tyPrism' <- importFrom "Data.Lens.Prism" $ importType "Prism'"
  prismFn <- importFrom "Data.Lens.Prism" $ importValue "prism"
  eitherRec <- importFrom "Data.Either"
    { leftCtor: importCtor "Either" "Left"
    , rightCtor: importCtor "Either" "Right"
    , ty: importType "Either"
    }
  traverse_ (genImportedType importMap) fields
  let
    declIdentifier = "_" <> ctorNameStr
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
            $ typeForall tyVars
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
      pure Set.empty
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
            $ typeForall tyVars
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
      pure $ extractReferencedLabelNames ty1

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
            $ typeForall tyVars
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
      pure $ Set.union (extractReferencedLabelNames ty1) (extractReferencedLabelNames ty2)

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
            $ typeForall tyVars
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
      pure $ Set.fromFoldable varNames

-- | Given `genLensProp ["label"]`, produces the following:
-- | ```
-- | _propLabel :: forall r a. Lens' { label :: a | r } a
-- | _propLabel = prop (Proxy :: Proxy "label")
-- | ```
genLensProp :: Partial => Set String -> Codegen Void Unit
genLensProp labelsInFile = do
  tyLens' <- importFrom "Data.Lens" $ importType "Lens'"
  prop <- importFrom "Data.Lens.Record" $ importValue "prop"
  proxy <- importFrom "Type.Proxy"
    { ctor: importCtor "Proxy" "Proxy"
    , ty: importType "Proxy"
    }
  for_ labelsInFile \label -> do
    let
      declIdentifier = "_prop" <> uppercaseFirstChar label
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
