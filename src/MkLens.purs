module MkLens where

import Prelude
import Prim hiding (Type, Row)

import Control.Monad.Free (runFree)
import Control.Monad.Writer (tell)
import Data.Array (mapWithIndex)
import Data.Array as Array
import Data.Char (fromCharCode, toCharCode)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String (splitAt, toUpper)
import Data.String.CodeUnits as String
import Data.Traversable (foldl, for, for_, traverse)
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
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (DataCtor(..), Declaration(..), Label(..), Labeled(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..), Proper(..), Row(..), Separated(..), Type(..), TypeVarBinding(..), Wrapped(..))
import Safe.Coerce (coerce)
import Tidy.Codegen (binderCtor, binderRecord, binderVar, caseBranch, declSignature, declValue, exprApp, exprCase, exprCtor, exprIdent, exprLambda, exprRecord, exprSection, exprTyped, printModule, typeApp, typeCtor, typeForall, typeRecord, typeString, typeVar)
import Tidy.Codegen.Monad (Codegen, importCtor, importFrom, importType, importValue, runCodegenTModule)
import Types (RecordLabelStyle(..))

type GenOptions =
  { genTypeAliasLens :: Boolean
  , genGlobalPropFile :: Maybe { filePath :: FilePath, moduleName :: String, overwrite :: Boolean }
  , recordLabelStyle :: RecordLabelStyle
  }

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
        modulePath = (getModulePath cst) <> ".Lens"
        Tuple labelNames generatedModule = runFree coerce $ unsafePartial $ runCodegenTModule modulePath do
          labelNames <- traverse (genOptic options) $ extractDecls cst
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
        FSA.writeTextFile UTF8 (Path.concat [ parentDir, "Lens.purs"]) outputtedContent
      pure labelNames
    _ ->
      liftEffect $ throw $
        "Parsing module for file path failed. Could not generate lens file for path: '" <> filePath <> "'"
  where
  getModulePath (Module { header: ModuleHeader { name: Name { name: ModuleName mn }}}) = mn

hasDecls :: Module Void -> Boolean
hasDecls (Module { body: ModuleBody { decls }}) = not $ Array.null decls

data DeclType
  = DTNewtype
      { tyName :: Name Proper
      , tyVars :: Array (TypeVarBinding Void)
      , wrappedTy :: Type Void
      }
  | DTType
      { tyName :: Name Proper
      , tyVars :: Array (TypeVarBinding Void)
      , aliasedTy :: Type Void
      }
  | DTData
      { tyName :: Name Proper
      , tyVars :: Array (TypeVarBinding Void)
      , constructors :: Array (DataCtor Void)
      }

extractDecls :: Module Void -> Array DeclType
extractDecls cst = foldMapModule visitor cst
  where
  visitor = defaultMonoidalVisitor
    { onDecl = case _ of
        DeclData ({ name, vars }) (Just (Tuple _ (Separated { head, tail }))) -> do
          Array.singleton $ DTData { tyName: name, tyVars: vars, constructors: Array.cons head $ map snd tail }
        DeclNewtype ({ name, vars }) _ _ ty -> do
          Array.singleton $ DTNewtype { tyName: name, tyVars: vars, wrappedTy: ty }
        DeclType ({ name, vars}) _ ty -> do
          Array.singleton $ DTType { tyName: name, tyVars: vars, aliasedTy: ty }
        _ -> mempty
    }

genOptic :: Partial => GenOptions -> DeclType -> Codegen Void (Set String)
genOptic opt = case _ of
  DTData rec -> case rec.constructors of
    -- This can never occur because `DeclData` case above only matches on `Just`
    -- which means there is at least one data constructor.
    [] ->
      pure Set.empty

    [ DataCtor { name: ctorName@(Name { name: Proper ctorNameStr }), fields } ] -> do
        genLensProduct opt rec.tyName ctorName rec.tyVars ctorNameStr fields

    _ -> do
      sets <- for rec.constructors \(DataCtor { name: ctorName@(Name { name: Proper ctorNameStr }), fields }) ->
        genPrismSum opt rec.tyName ctorName rec.tyVars ctorNameStr fields
      pure $ foldl Set.union Set.empty sets

  DTNewtype rec@{ tyName: Name { name: Proper tn }} -> do
    tyLens' <- importFrom "Data.Lens" $ importType "Lens'"
    lensNewtype <- importFrom "Data.Lens.Iso.Newtype" $ importValue "_Newtype"
    let
      declIdentifier = "_" <> tn
    tell
      [ declSignature declIdentifier
          $ typeForall rec.tyVars
          $ typeApp (typeCtor tyLens')
              [ (typeApp (typeCtor rec.tyName) $ map tyVarToTypeVar rec.tyVars)
              , rec.wrappedTy
              ]
      , declValue declIdentifier [] (exprIdent lensNewtype)
      ]
    case rec.wrappedTy of
      TypeRecord (Wrapped { value: Row { labels: Just (Separated { head, tail })}}) -> do
        pure $ foldl (\acc next -> Set.insert (unLabel $ snd next) acc) (Set.singleton $ unLabel head) tail
      _ ->
        pure Set.empty

  DTType rec@{ tyName: Name { name: Proper tn }, aliasedTy } -> do
    when opt.genTypeAliasLens do
      identity_ <- importFrom "Prelude" $ importValue "identity"
      tyLens' <- importFrom "Data.Lens" $ importType "Lens'"
      let
        declIdentifier = "_" <> tn
      tell
        [ declSignature declIdentifier
            $ typeForall rec.tyVars
            $ typeApp (typeCtor tyLens')
                [ (typeApp (typeCtor rec.tyName) $ map tyVarToTypeVar rec.tyVars)
                , rec.aliasedTy
                ]
        , declValue declIdentifier [] (exprIdent identity_)
        ]
    case aliasedTy of
      TypeRecord (Wrapped { value: Row { labels: Just (Separated { head, tail })}}) -> do
        pure $ foldl (\acc next -> Set.insert (unLabel $ snd next) acc) (Set.singleton $ unLabel head) tail
      _ ->
        pure Set.empty

unLabel :: forall a. Labeled (Name Label) a -> String
unLabel (Labeled { label: Name { name: Label label } }) = label

tyVarToTypeVar :: TypeVarBinding Void -> Type Void
tyVarToTypeVar = case _ of
  TypeVarName n -> typeVar n
  TypeVarKinded (Wrapped { value: Labeled { label: n }}) -> typeVar n

genLensProduct
  :: Partial
  => GenOptions
  -> Name Proper
  -> Name Proper
  -> Array (TypeVarBinding Void)
  -> String
  -> Array (Type Void)
  -> Codegen Void (Set String)
genLensProduct opt tyName ctorName tyVars ctorNameStr fields = do
  tyLens' <- importFrom "Data.Lens" $ importType "Lens'"
  lens <- importFrom "Data.Lens.Lens" $ importValue "lens"
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
            exprApp (exprIdent lens)
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
            exprApp (exprIdent lens)
              [ exprLambda [ binderCtor ctorName [ binderVar "a"] ] (exprIdent "a")
              , exprCtor ctorName
              ]
        ]
      pure Set.empty

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
            exprApp (exprIdent lens)
              [ exprLambda [ binderCtor ctorName [ binderVar "a", binderVar "b"] ]
                  (exprApp (exprCtor tupleRec.ctor) [ exprIdent "a", exprIdent "b" ])
              , exprLambda [ binderCtor tupleRec.ctor [ binderVar "a", binderVar "b"] ]
                  (exprApp (exprCtor ctorName) [ exprIdent "a", exprIdent "b" ])
              ]
        ]
      pure Set.empty
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
            exprApp (exprIdent lens)
              [ exprLambda [ binderCtor ctorName $ map binderVar varNames ]
                  (exprRecord $ map (\var -> Tuple var (exprIdent var)) varNames )
              , exprLambda [ binderRecord varNames ]
                  (exprApp (exprCtor ctorName) $ map exprIdent varNames)
              ]
        ]
      pure $ Set.fromFoldable varNames

genPrismSum
  :: Partial
  => GenOptions
  -> Name Proper
  -> Name Proper
  -> Array (TypeVarBinding Void)
  -> String
  -> Array (Type Void)
  -> Codegen Void (Set String)
genPrismSum opt tyName ctorName tyVars ctorNameStr fields = do
  tyPrism' <- importFrom "Data.Lens.Prism" $ importType "Prism'"
  prismFn <- importFrom "Data.Lens.Prism" $ importValue "prism"
  eitherRec <- importFrom "Data.Either"
    { leftCtor: importCtor "Either" "Left"
    , rightCtor: importCtor "Either" "Right"
    , ty: importType "Either"
    }
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
      pure Set.empty

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
                    (exprApp (exprCtor eitherRec.rightCtor) [ exprIdent "a", exprIdent "b" ])
                , caseBranch [ binderVar "other" ]
                    (exprApp (exprCtor eitherRec.leftCtor) [ exprIdent "other"])
                ]
              ]
        ]
      pure Set.empty

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
                    (exprApp (exprCtor eitherRec.rightCtor)
                      [ exprRecord $ map (\var -> Tuple var (exprIdent var)) varNames ])
                , caseBranch [ binderVar "other" ]
                    (exprApp (exprCtor eitherRec.leftCtor) [ exprIdent "other"])
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
      :: ({ nextLabel :: Int
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
          })
      -> { recordTy :: Array (Tuple String (Type Void)), varNames :: Array String }
    foldlFn f = do
      let { recordTy, varNames } = foldl f initial fields
      { recordTy, varNames }
