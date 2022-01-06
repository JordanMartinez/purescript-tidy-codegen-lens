# purescript-tidy-codegen-lens

Generate lenses and prisms for your data types automatically.

An input file, `./Foo.purs`, will generate an output file `./Foo/Lens.purs` if `Foo.purs` contains any type declarations.

This is currently a WIP.

## Usage

For testing a local version, run `spago build` and then `./tidy-mklens.js <CLI args>`. To build a bundled one, use `./build-tidy-mklens.sh`.

```
$ ./tidy-mklens.js --help
tidy-mklens
    A CLI for generating optics for your data types

    Expected usage:
      tidy-mklens [OPTIONS] PURS_GLOBS...

    Examples:
      tidy-mklens src
      tidy-mklens --global-record-lens-module RecordLens src
      tidy-mklens --label-style-abc src
      tidy-mklens --gen-type-alias-lenses src
      tidy-mklens --output-dir src .spago/*/*/src/**/*.purs:4

    --gen-type-alias-isos,-t                      Generate isos for type aliases
    --global-record-lens-module,-m MODULE_PATH    The full module path to use for the single record label lenses file
                                                  (e.g `Foo.Bar.Lens`). The module will be outtputed to a file based
                                                  on the module path (e.g. `Foo.Bar.Lens` will be saved to
                                                  `<outputDir>/Foo/Bar/Lens.purs`).
    --help,-h                                     Show this help message.
    --label-prefix,-l PREFIX                      Use `_PREFIXFoo` for the lens for a record '{ foo :: a }'
    --label-prefix-none,-n                        Use '_foo' for the lens for a record '{ foo :: a }'
    --label-style-abc,-b                          Data constructors with 3+ args will use record labels based on the alphabet (e.g. 'a', 'b', ..., 'z', 'aa', 'ab', ...)
    --label-style-arg,-a                          Data constructors with 3+ args will use record labels of 'argN' (e.g. 'arg1', 'arg2', ..., 'argN')
    --output-dir,-o                               The directory into which to write the generated files
                                                  (defaults to `src`).
    --version,-v                                  Shows the current version

    GLOB[:DIR_STRIP_COUNT]                        Globs for PureScript sources (e.g. `src` `test/**/*.purs`)
                                                  and the number of root directories to strip from each file
                                                  path (defaults to 1) that are separated by the OS-specific
                                                  path delimiter (POSIX: ':', Windows: ';')
```

## Examples

The table illustrates some of the CLI args above via the current output on the files stored in the [snapshots folder](./snapshots). You can verify that these compile via `spago -x snapshots.dhall build --purs-args "--output output-snapshots"`.

| Purpose | Files |
| - | - |
| Show the 'arg' label style | <ul><li><p>Source file</p><ul><li>[UseArgLabelStyle.purs](./snapshots/UseArgLabelStyle.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseArgLabelStyle/Lens.purs](./snapshots/UseArgLabelStyle/Lens.purs)</li></ul></li></ul> |
| Show the 'abc' label style | <ul><li><p>Source file</p><ul><li>[UseAbcLabelStyle.purs](./snapshots/UseAbcLabelStyle.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseAbcLabelStyle/Lens.purs](./snapshots/UseAbcLabelStyle/Lens.purs)</li></ul></li></ul> |
| Show the `--gen-type-alias-isos` option | <ul><li><p>Source file</p><ul><li>[UseTypeAliases.purs](./snapshots/UseTypeAliases.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseTypeAliases/Lens.purs](./snapshots/UseTypeAliases/Lens.purs)</li></ul></li></ul> |
| Show the `--label-prefix-none` option | <ul><li><p>Source file</p><ul><li>[UseNoLabelPrefix.purs](./snapshots/UseNoLabelPrefix.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseNoLabelPrefix/Lens.purs](./snapshots/UseNoLabelPrefix/Lens.purs)</li></ul></li></ul> |
| Show the `--label-prefix "prop"` option (default) | <ul><li><p>Source file</p><ul><li>[UseLabelPrefix.purs](./snapshots/UseLabelPrefix.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseLabelPrefix/Lens.purs](./snapshots/UseLabelPrefix/Lens.purs)</li></ul></li></ul> |
| The record label lenses will be duplicated in each module. This is why the `--gen-record-lens-*` options exist | <ul><li><p>Source file</p><ul><li>[UseLocalProps/Module1.purs](./snapshots/UseLocalProps/Module1.purs)</li><li>[UseLocalProps/Module2.purs](./snapshots/UseLocalProps/Module2.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseLocalProps/Module1/Lens.purs](./snapshots/UseLocalProps/Module1/Lens.purs)</li><li>[UseLocalProps/Module2/Lens.purs](./snapshots/UseLocalProps/Module2/Lens.purs)</li></ul></li></ul> |
| Demo the `--gen-record-lens-*` options | <ul><li><p>Source file</p><ul><li>[UseGlobalPropFile/Module1.purs](./snapshots/UseGlobalPropFile/Module1.purs)</li><li>[UseGlobalPropFile/Module2.purs](./snapshots/UseGlobalPropFile/Module2.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseGlobalPropFile/GlobalRecordLens.purs](./snapshots/UseGlobalPropFile/GlobalRecordLens.purs)</li></ul></li></ul> |
| Primary example | <ul><li><p>Source file</p><ul><li>[PrimaryExample.purs](./snapshots/PrimaryExample.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[PrimaryExample/Lens.purs](./snapshots/PrimaryExample/Lens.purs)</li></ul></li></ul> |

Files were generated using the below commands, which are stored in [regen-snapshots.sh](./regen-snapshots.sh):

```bash
# Note: the `arg` style is the default
./tidy-mklens.js --label-style-arg --output-dir snapshots snapshots/UseArgLabelStyle.purs

./tidy-mklens.js --label-style-abc --output-dir snapshots snapshots/UseAbcLabelStyle.purs

# Can optionally generate lenses for type aliases
# Useful when just getting familiar with a library and type signatures
# and typed holes are all you have
./tidy-mklens.js --gen-type-alias-isos --output-dir snapshots snapshots/UseTypeAliases.purs

# By default, any record labels referenced in your types will
# have their corresponding lense generated using the style
# `_propLabelName`. You can swap out 'prop' for your own
# custom prefix...
./tidy-mklens.js --label-prefix "prop" --output-dir snapshots snapshots/UseLabelPrefix.purs

# ... or none at all (e.g. `_labelName`).
./tidy-mklens.js --label-prefix-none --output-dir snapshots snapshots/UseNoLabelPrefix.purs

# If the same label is used in multiple types,
# a lens for that label will be stored in each file,
# thereby duplicating the lens. This can lead to
# import frency.

./tidy-mklens.js --output-dir snapshots snapshots/UseLocalProps/

# One way around this is to generate a single file
# that stores all lenses for the deduplicated labels,
# ensuring that label lenses are only imported from
# one place.
./tidy-mklens.js \
  --output-dir snapshots \
  --global-record-lens-module UseGlobalPropFile.GlobalRecordLens \
  snapshots/UseGlobalPropFile

# Here's the primary example, showing the full power of the code
./tidy-mklens.js --gen-type-alias-isos  --output-dir snapshots snapshots/PrimaryExample.purs

# Open imports aren't always handled correctly. See these snapshots
./tidy-mklens.js --output-dir snapshots snapshots/CheckOpenImports
```

### Explaining the `GLOB[:DIR_STRIP_COUNT]` arg

The generated file's file path is based on the input file's file path. Most of the time, one will use this program to generate optics for the `src` directory. However, in monorepos, there may be multiple directories that contains PureScript source code. This feature exists to account for those use cases.

The `glob:<int>` indicates how many parent directories, starting from the file's relative path's root, to strip from the outputted file's file path. When not specified, the `DIR_STIP_COUNT` defaults to `1`.

For example, let's say the `src` directory has the following structure:
```
/src
  /Foo
    /Bar.purs
```

Running `./tidy-mklens.js --output-dir lenses src:X` where `X` is one of the integers listed in the below table will produce the corresponding output:

| X |  Output file | Explanation |
| - | - | - |
| 1<br />(default) | `lenses/Foo/Bar/Lens.purs` | the `src` segment was removed |
| 2 | `lenses/Bar/Lens.purs` | the `src` and `Foo` segments were removed |
| 3 | `lenses/Lens.purs` | the `src`, `Foo`, and `Bar` segments were removed |
| 4 | `lenses/Lens.purs` | Since there are only 3 segments available, this is no different than when X is 3 |

This feature enables one to run this program against all of a projects dependencies (as stored in the `.spago` folder) and output the results into a new folder. This can be accomplished via the glob `.spago/*/*/src/**/*.purs:4`, which says, "If you come across a file `.spago/packageName/version/src/Foo/Bar.purs`, strip the first four root directories (e.g. `.spago/packageName/version/src`), and append the result (e.g. `Foo/Bar.purs`) to the output directory (e.g. `<outputDir>/Foo/Bar.purs`).

For example, the `lenses` folder contains the output of running this command, which is stored in [regen-lenses.sh](./regen-lenses.sh):
```sh
./tidy-mklens.js \
  --output-dir lenses \
  --gen-type-alias-isos \
  --label-prefix-none \
  --global-record-lens-module Dependencies.RecordLens \
  .spago/*/*/src/**/*.purs:4
```

## Assumptions

**In general, the generated `Lens.purs` file(s) will compile so long as the source file compiles and does not contain any warnings regarding your imports. However, the generated file may produce compiler warnings.**

`tidy-mklens` uses limited information to generate a `Lens.purs` file. There are some ambiguous situations it cannot handle without more information (e.g. type checking).

More specifically, the generated `Lens.purs` file will compile if the following is true about your source file:
```purescript
module ModuleName where

-- Assumption #1:
-- To ensure types referenced in the source file's types are imported
-- in the generated file, always re-import all open imports.
--
-- If the types referenced in the source file's types are from imports,
-- the generated file will not compile unless they are imported
-- in the generated file, too.
--
-- However, this will produce compiler warnings in the generated file
-- if the open imports' members aren't used.
-- `tidy-mklens` can't know what these modules import
-- and thus whether they would be used in the generated file.
--
-- Note: the compiler warns if 2+ open imports are used.
import Prelude
import SomethingElse
import MyModule hiding (someMember)

-- Assumption #2:
-- Each module alias to an open import refers to at most one module.
--
-- Without this constraint, `tidy-mklens` cannot know which module
-- (e.g. `Module1` or `Module2`) to import in the generated file.
--
-- Note: the compiler warns if an alias refers to 2+ open imports.
import ImportWithModuleAlias as ThisIsOk
import SomeModule hiding (someMember) as ThisIsAlsoOk
import Module1 as ThisIsBadBecauseItRefersToMultipleModules
import Module2 as ThisIsBadBecauseItRefersToMultipleModules

-- That being said, module aliases to closed imports are ok,
-- even if one alias refers to the multipe modules.
--
-- All usages of such members will use the qualified notation
-- for them (e.g. `Mod.Type1`), so `tidy-mklens` can determine
-- which module to import to ensure that type is included
-- in the generated `Lens.purs` file.
import ModuleA (Type1, Type2) as Mod
import ModuleB (Type3, Type4) as Mod

-- Explicit imports work fine without issues
import ModuleWithExplicitExports (Foo, bar, baz)
```
