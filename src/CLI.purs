module CLI where

import Prelude

import ArgParse.Basic (ArgError, anyNotFlag, argument, boolean, choose, default, flag, flagHelp, flagInfo, fromRecord, optional, parseArgs, unfolded1, unformat)
import Control.Monad.Error.Class (throwError)
import Data.Array (fold)
import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (for_)
import Data.FoldableWithIndex (findWithIndex)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith)
import Data.String as String
import Data.String.NonEmpty.Internal (NonEmptyString, liftS)
import Data.String.NonEmpty.Internal as NES
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Node.Path (FilePath)
import Node.Path as Path
import Types (RecordLabelStyle(..))
import Version (versionStr)

type CliArgs =
  {
    -- | Some type alises refer to records
    -- | ```
    -- | type Foo = { a :: Int }
    -- | ```
    -- | So, a lens is generated for the `a` label.
    -- |
    -- | Other type aliases refer to non record types
    -- | ```
    -- | type Foo = Wrapped (Label (Name Proper) (Type Void))
    -- | ```
    -- | It can be hard to know what `Foo` refers to. So
    -- | rather than remembering to write `_Wrapped` when one
    -- | comes across a `Foo`, one can write `_Foo`, ask the compiler
    -- | for help via typed holes, and then be directed by the compiler
    -- | to use `_Wrapped`.
    -- | The intermediary `_Foo` can be removed once
    -- | one becomes more familiar with a libary's type aliases.
    genTypeAliasLens :: Boolean
  ,
    -- | Multiple types may refer to the same label name
    -- | across multiple files...
    -- | ```
    -- | // Foo.purs
    -- | type Foo = { val :: Int }
    -- | type Bar = { val :: String }
    -- | // Baz.purs
    -- | type Baz = { val :: Char }
    -- | ```
    -- | By default, each module's `Lens.purs` file
    -- | will include a lens for each label referenced
    -- | in that file. However, this can lead to
    -- | importing lenses from different modules
    -- | that do the same thing:
    -- | ```
    -- | import Foo (_propA)
    -- | import Baz (_propA)
    -- |
    -- | // where `_propA` is
    -- | _propVal :: forall a r. Lens' { val :: a | r } a
    -- | _propVal :: prop (Proxy :: Proxy "val")
    -- | ```
    -- |
    -- | Enabling this feature means a single `RecordLens.purs`
    -- | file will be generated where each label's lens
    -- | is generated only once.
    genGlobalPropFile :: Maybe { filePath :: FilePath, modulePath :: String }

  ,
    -- | By default, this will be "prop", so that a record label `{ foo :: a }`
    -- | will have the `_propFoo` generated for it.
    -- | If this value is empty, the label generated will instead be
    -- | `_foo`.
    labelPrefix :: Maybe NonEmptyString

  ,
    -- | When there are data constructors with 3+ arguments
    -- | we could convert the value into a nested `Tuple`
    -- | but that seems less user-friendly than just using
    -- | a record.
    -- | However, what should the record's labels be?
    -- | We can use two styles:
    -- | - `AlphabetRecordLabels`: `Foo a b c d -> { a, b, c, d }`
    -- | - `ArgRecordLabels`: `Foo a b c d -> { arg1: a, arg2: b, arg3: c, arg4: d }`
    recordLabelStyle :: RecordLabelStyle
  , outputDir :: String
  }

type PrefixedGlob =
  { glob :: String
  , dirCount :: Int
  }

parseCliArgs :: Array String -> Either ArgError { prefixedGlobs :: Array PrefixedGlob, options :: CliArgs }
parseCliArgs =
  parseArgs
    "tidy-mklens"
    ( joinWith "\n"
        [ "A CLI for generating optics for your data types"
        , ""
        , "Expected usage: "
        , "  tidy-mklens [OPTIONS] PURS_GLOBS..."
        , ""
        , "Examples:"
        , "  tidy-mklens src"
        , "  tidy-mklens --global-record-lens-module RecordLens src"
        , "  tidy-mklens --label-style-abc src"
        , "  tidy-mklens --gen-type-alias-lenses src"
        , "  tidy-mklens --output-dir src .spago/*/*/src/**/*.purs:4"
        ]
    )
    parser
  where
  parser =
    { prefixedGlobs: _, options: _ }
      <$> pursGlobs
      <*> fromRecord
            { genTypeAliasLens
            , genGlobalPropFile
            , recordLabelStyle
            , labelPrefix
            , outputDir
            }
      <* flagInfo [ "--version", "-v" ] "Shows the current version" versionStr
      <* flagHelp

  labelPrefix =
    choose "Label prefix"
      [ Nothing <$ flag [ "--label-prefix-none", "-n" ] "Use '_foo' for the lens for a record '{ foo :: a }'"
      , argument [ "--label-prefix", "-l" ] "Use `_PREFIXFoo` for the lens for a record '{ foo :: a }'"
          # unformat "PREFIX" validatePrefix
      ]
      # default (NES.fromString "prop")
    where
    validatePrefix s = do
      case NES.fromString s of
        Nothing -> do
          throwError $ "Invalid label prefix. Prefix must not be empty"
        x@(Just nes) -> do
          unless (liftS (test alphaNumUnderscoreRegex) nes) do
            throwError $ "Invalid label prefix. '" <> s <> "' must pass the '^[a-zA-Z0-9_]+$' regex."
          pure x

      where
      alphaNumUnderscoreRegex = unsafeRegex "^[a-zA-Z0-9_]+$" noFlags

  genTypeAliasLens =
    flag
      [ "--gen-type-alias-isos", "-t" ]
      "Generate isos for type aliases"
      # boolean

  genGlobalPropFile =
    optional
      ( argument [ "--global-record-lens-module", "-m" ] description
          # unformat "MODULE_PATH" validateModulePath
      )
    where
    description = joinWith ""
      [ "The full module path to use for the single record label lenses file (e.g `Foo.Bar.Lens`). "
      , "The module will be outtputed to a file based on the module path (e.g. `Foo.Bar.Lens` "
      , "will be saved to `<outputDir>/Foo/Bar/Lens.purs`)."
      ]
    validateModulePath s = do
      when (s == "") do
        throwError "Invalid module path. Module path must not be an empty string"
      let
        segments = String.split (Pattern ".") s
        alphaNumUnderscoreCheck = segments # findWithIndex \_ next ->
          not $ test alphaNumUnderscoreRegex next
        firstCharCheck = segments # findWithIndex \_ -> not $ firstCharIsUppercase

      for_ alphaNumUnderscoreCheck \r ->
        throwError $ "Invalid module path. Segment at index " <> show r.index <> ", '" <> r.value <> "', does not pass `[a-zA-Z0-9_]+` regex check"

      for_ firstCharCheck \r ->
        throwError $ "Invalid module path. First character for segment at index " <> show r.index <> ", '" <> r.value <> "', is not uppercased"

      case Array.unsnoc segments of
        Nothing ->
          throwError "Invalid module path. Module path does not contain any segments."
        Just { init, last } ->
          pure { modulePath: s, filePath: Path.concat $ Array.snoc init $ last <> ".purs" }

    firstCharIsUppercase s = do
      let firstChar = String.take 1 s
      firstChar == "_" || firstChar == String.toUpper firstChar

    alphaNumUnderscoreRegex = unsafeRegex "^[a-zA-Z0-9_]+$" noFlags

  recordLabelStyle =
    choose "Record label style"
      [ ArgRecordLabels <$ flag [ "--label-style-arg", "-a" ]
          "Data constructors with 3+ args will use record labels of 'argN' (e.g. 'arg1', 'arg2', ..., 'argN')"
      , AlphabetRecordLabels <$ flag [ "--label-style-abc", "-b" ]
          "Data constructors with 3+ args will use record labels based on the alphabet (e.g. 'a', 'b', ..., 'z', 'aa', 'ab', ...)"
      ]
      # default ArgRecordLabels

  outputDir =
    argument [ "--output-dir", "-o" ] "The directory into which to write the generated files (defaults to `src`)."
      # default "src"

  pursGlobs =
    anyNotFlag globExample description
      # unformat globExample validate
      # unfolded1
    where
    description = joinWith ""
      [ "Globs for PureScript sources (e.g. `src` `test/**/*.purs`) "
      , "and the number of root directories to strip from each file path (defaults to 1) "
      , "that are separated by the OS-specific path delimiter (POSIX: ':', Windows: ';'), "
      ]
    delimit l r = l <> Path.delimiter <> r
    globExample = delimit "GLOB[" "DIR_STRIP_COUNT]"
    validate s = do
      case String.split (String.Pattern Path.delimiter) s of
        [ glob, dirStripCount ]
          | Just dirCount <- Int.fromString dirStripCount -> pure { glob, dirCount }
          | otherwise -> throwError $ fold
              [ "Invalid source glob. Expected directory strip count to be an integer "
              , "but was '"
              , s
              , "'"
              ]

        [ glob ] -> pure { glob, dirCount: 1 }
        _ -> throwError $ joinWith ""
          [ "Invalid source glob. Expected either a glob (e.g. `src`) "
          , "or a glob and the prefix to strip separated by a '"
          , Path.delimiter
          , "' character "
          , "(e.g. `"
          , delimit ".spago/*/*/src/**/*.purs" "4"
          , "`)"
          ]
