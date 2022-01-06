module Main where

import Prelude

import ArgParse.Basic (ArgError(..), ArgErrorMsg(..), printArgError)
import CLI (PrefixedGlob, parseCliArgs)
import Data.Array (drop)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (foldl, for_, surround)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.String (Pattern(..), joinWith, stripSuffix)
import Data.String as String
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref
import MkLens (FileInfo, genLensProp, generateLensModule)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSA
import Node.FS.Stats as Stats
import Node.Glob.Basic (expandGlobsWithStatsCwd)
import Node.Path (dirname)
import Node.Path as Path
import Node.Process (argv)
import Node.Process as Process
import Partial.Unsafe (unsafePartial)
import Tidy.Codegen (printModule)
import Tidy.Codegen.Monad (codegenModule)

main :: Effect Unit
main = do
  args <- map (drop 2) argv
  case parseCliArgs args of
    Left e -> do
      log $ printArgError e
      case e of
        ArgError _ ShowHelp ->
          Process.exit 0
        ArgError _ (ShowInfo _) ->
          Process.exit 0
        _ ->
          Process.exit 1
    Right { prefixedGlobs, options } -> launchAff_ do
      files <- globsToFileInfo options.outputDir prefixedGlobs
      labelSets <- traverse (generateLensModule options) files
      for_ options.genGlobalPropFile \{ filePath, modulePath } -> do
        let
          labelNameSet = Array.foldl Set.union Set.empty labelSets
          globalPropFile = Path.concat [ options.outputDir, filePath ]
        unless (Set.isEmpty labelNameSet) do
          let
            content = printModule $ unsafePartial $ codegenModule modulePath do
              genLensProp options.labelPrefix labelNameSet
          unlessM (FSA.exists $ dirname globalPropFile) do
            FSA.mkdir $ dirname globalPropFile
          FSA.writeTextFile UTF8 globalPropFile content

globsToFileInfo :: String -> Array PrefixedGlob -> Aff (Array FileInfo)
globsToFileInfo outputDir = map join <<< traverse \{ glob, dirCount } -> do
  globFiles <- expandGlobs [ glob ]

  outputFilesRef <- liftEffect $ Ref.new Map.empty

  fileInfo <- for globFiles \file -> do
    let
      originalDirPlusFile = Path.concat
        [ Path.dirname file
        , Path.basenameWithoutExt file (Path.extname file)
        ]
      -- Per the Node docs
      -- (https://nodejs.org/api/path.html#pathrelativefrom-to),
      -- if an empty string is passed in as the 'from'
      -- then the current working directory will be used.
      relativePath = Path.relative "" originalDirPlusFile
      outputDirOutputFile = Path.concat
        $ Array.cons outputDir
        $ flip Array.snoc "Lens.purs"
        $ drop dirCount
        $ String.split (String.Pattern Path.sep) relativePath

    liftEffect $ flip Ref.modify_ outputFilesRef \m ->
      Map.alter
        case _ of
          Nothing -> Just $ Set.singleton file
          Just prev -> Just $ Set.insert file prev
        outputDirOutputFile
        m
    pure { inputFile: file, outputFile: outputDirOutputFile }

  liftEffect do
    outputtedFiles <- Ref.read outputFilesRef
    for_ (NEA.fromArray $ Map.toUnfoldable $ Map.filter (\r -> Set.size r /= 1) outputtedFiles) \nea -> do
      throw
        $ joinWith ""
            [ "Error in source glob: '"
            , glob
            , "'. Error in DIR_STRIP_COUNT arg: '"
            , show dirCount
            , "'. Found a case where the following input files' generated optics content would be "
            , "outputted into the same file. Later file's output would overwrite earlier file's output. "
            , "To fix, decrease the DIR_STRIP_COUNT part of the source glob."
            , foldl (<>) "\n" $ nea <#> \(Tuple outFile inputFiles) -> joinWith "\n"
                [ "Output file: "
                , "\t" <> outFile
                , append "Input files:" $ surround "\n\t" inputFiles
                ]
            ]

  pure fileInfo

-- Note: credit for the below code goes to @natefaubion
-- since this was copied from `natefaubion/purescript-tidy`
expandGlobs :: Array String -> Aff (Array String)
expandGlobs = map dirToGlob >>> expandGlobsWithStatsCwd >>> map onlyFiles
  where
  dirToGlob path =
    if Path.extname path == "" then
      if isJust (stripSuffix (Pattern "**") path) then
        Path.concat [ path, "*.purs" ]
      else
        Path.concat [ path, "**", "*.purs" ]
    else
      path

  onlyFiles =
    Map.filter Stats.isFile
      >>> Map.keys
      >>> Set.toUnfoldable
