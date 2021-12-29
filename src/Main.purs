module Main where

import Prelude

import ArgParse.Basic (ArgError(..), ArgErrorMsg(..), printArgError)
import CLI (parseCliArgs)
import Data.Array (drop)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (isJust)
import Data.Set as Set
import Data.String (Pattern(..), stripSuffix)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)
import MkLens (genLensProp, generateLensModule)
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
    Right { genTypeAliasLens, genGlobalPropFile, recordLabelStyle, pursGlobs } -> launchAff_ do
      files <- expandGlobs pursGlobs
      labelSets <- traverse (generateLensModule { genTypeAliasLens, genGlobalPropFile, recordLabelStyle }) files
      for_ genGlobalPropFile \{ filePath, moduleName, overwrite } -> do
        let labelNameSet = Array.foldl Set.union Set.empty labelSets
        unless (Set.isEmpty labelNameSet) do
          let
            content = printModule $ unsafePartial $ codegenModule moduleName do
              genLensProp labelNameSet
          alreadyExists <- FSA.exists filePath
          unless (alreadyExists && not overwrite) do
            unlessM (FSA.exists $ dirname filePath) do
              FSA.mkdir $ dirname filePath
            FSA.writeTextFile UTF8 filePath content

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
