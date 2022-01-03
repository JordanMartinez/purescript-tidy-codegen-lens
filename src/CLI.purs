module CLI where

import Prelude

import ArgParse.Basic (ArgError, anyNotFlag, argument, boolean, choose, default, flag, flagHelp, flagInfo, fromRecord, optional, parseArgs, unfolded1, unformat)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either)
import Data.Foldable (for_)
import Data.FoldableWithIndex (findWithIndex)
import Data.Maybe (Maybe)
import Data.String (Pattern(..), joinWith)
import Data.String as String
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Node.Path (FilePath, basenameWithoutExt, extname)
import Types (RecordLabelStyle(..))

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
    genGlobalPropFile :: Maybe { filePath :: FilePath, moduleName :: String, overwrite :: Boolean }

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
  , pursGlobs :: Array String
  }

versionStr :: String
versionStr = "v0.3.0"

parseCliArgs :: Array String -> Either ArgError CliArgs
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
        , "  tidy-mklens -w -p src/RecordLens.purs -m RecordLens src"
        , "  tidy-mklens --label-style-abc src"
        , "  tidy-mklens --gen-type-alias-lenses src"
        ]
    )
    parser
  where
  parser =
    fromRecord
      { genTypeAliasLens
      , genGlobalPropFile
      , recordLabelStyle
      , pursGlobs
      }
      <* flagInfo [ "--version", "-v" ] "Shows the current version" versionStr
      <* flagHelp

  genTypeAliasLens =
    flag
      [ "--gen-type-alias-isos", "-t" ]
      "Generate isos for type aliases"
      # boolean
  genGlobalPropFile =
    optional
      ( fromRecord
          { filePath:
              argument [ "--global-record-lens-file", "-p" ] "Output record label lenses to this single file rather than in each module's file (e.g. `src/RecordLens.purs`)"
                # unformat "FILE_PATH" validateFilePath
          , moduleName:
              argument [ "--global-record-lens-module", "-m" ] "The full module path to use for the single record label lenses file (e.g `Foo.Bar.Lens`)"
                # unformat "MODULE_PATH" validateModulePath
          , overwrite:
              flag [ "--global-record-lens-overwrite-file", "-w" ] "Overwrite the single file if it already exists"
                # boolean
          }
      )
    where
    validateFilePath s = do
      when (s == "") do
        throwError "Invalid file path. File path must not be an empty string"
      when (extname s /= ".purs") do
        throwError "Invalid file path. File path must end with `.purs` file extension"
      when (basenameWithoutExt s ".purs" == "") do
        throwError "Invalid file path. File path's base name must not be empty"
      pure s

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

      pure s

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
  pursGlobs =
    anyNotFlag "PURS_GLOBS" "Globs for PureScript sources."
      # unfolded1
