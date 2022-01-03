# purescript-tidy-codegen-lens

Generate lenses and prisms for your data types automatically.

An input file, `./Foo.purs`, will generate an output file `./Foo/Lens.purs` if `Foo.purs` contains any type declarations.

This is currently a WIP.

## Usage

Run `spago build` and then `./tidy-mklens <CLI args>`

```
$ ./tidy-mklens --help
tidy-mklens
    A CLI for generating optics for your data types

    Expected usage:
      tidy-mklens [OPTIONS] PURS_GLOBS...

    Examples:
      tidy-mklens src
      tidy-mklens -w -p src/RecordLens.purs -m RecordLens src
      tidy-mklens --label-style-abc src
      tidy-mklens --gen-type-alias-lenses src

    --gen-type-alias-isos,-t                      Generate isos for type aliases
    --global-record-lens-file,-p FILE_PATH        Output record label lenses to this single file rather than in each module's file (e.g. `src/RecordLens.purs`)
    --global-record-lens-module,-m MODULE_PATH    The full module path to use for the single record label lenses file (e.g `Foo.Bar.Lens`)
    --global-record-lens-overwrite-file,-w        Overwrite the single file if it already exists
    --help,-h                                     Show this help message.
    --label-style-abc,-b                          Data constructors with 3+ args will use record labels based on the alphabet (e.g. 'a', 'b', ..., 'z', 'aa', 'ab', ...)
    --label-style-arg,-a                          Data constructors with 3+ args will use record labels of 'argN' (e.g. 'arg1', 'arg2', ..., 'argN')
    --version,-v                                  Shows the current version

    PURS_GLOBS                                    Globs for PureScript sources.
```

## Examples

The table illustrates some of the CLI args above via the current output on the files stored in the [snapshots folder](./snapshots). You can verify that these compile via `spago -x snapshots.dhall build`.

| Purpose | Files |
| - | - |
| Show the 'arg' label style | <ul><li><p>Source file</p><ul><li>[UseArgLabelStyle.purs](./snapshots/UseArgLabelStyle.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseArgLabelStyle/Lens.purs](./snapshots/UseArgLabelStyle/Lens.purs)</li></ul></li></ul> |
| Show the 'abc' label style | <ul><li><p>Source file</p><ul><li>[UseAbcLabelStyle.purs](./snapshots/UseAbcLabelStyle.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseAbcLabelStyle/Lens.purs](./snapshots/UseAbcLabelStyle/Lens.purs)</li></ul></li></ul> |
| Show the `--gen-type-alias-isos` option | <ul><li><p>Source file</p><ul><li>[UseTypeAliases.purs](./snapshots/UseTypeAliases.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseTypeAliases/Lens.purs](./snapshots/UseTypeAliases/Lens.purs)</li></ul></li></ul> |
| The record label lenses will be duplicated in each module. This is why the `--gen-record-lens-*` options exist | <ul><li><p>Source file</p><ul><li>[UseLocalProps/Module1.purs](./snapshots/UseLocalProps/Module1.purs)</li><li>[UseLocalProps/Module2.purs](./snapshots/UseLocalProps/Module2.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseLocalProps/Module1/Lens.purs](./snapshots/UseLocalProps/Module1/Lens.purs)</li><li>[UseLocalProps/Module2/Lens.purs](./snapshots/UseLocalProps/Module2/Lens.purs)</li></ul></li></ul> |
| Demo the `--gen-record-lens-*` options | <ul><li><p>Source file</p><ul><li>[UseGlobalPropFile/Module1.purs](./snapshots/UseGlobalPropFile/Module1.purs)</li><li>[UseGlobalPropFile/Module2.purs](./snapshots/UseGlobalPropFile/Module2.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseGlobalPropFile/GlobalRecordLens.purs](./snapshots/UseGlobalPropFile/GlobalRecordLens.purs)</li></ul></li></ul> |
| Primary example | <ul><li><p>Source file</p><ul><li>[PrimaryExample.purs](./snapshots/PrimaryExample.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[PrimaryExample/Lens.purs](./snapshots/PrimaryExample/Lens.purs)</li></ul></li></ul> |

Files were generated using the below commands, which are stored in [regen-snapshots.sh](./regen-snapshots.sh):

```bash
# Note: the `arg` style is the default
./tidy-mklens --label-style-arg snapshots/UseArgLabelStyle.purs

./tidy-mklens --label-style-abc snapshots/UseAbcLabelStyle.purs

# Can optionally generate lenses for type aliases
# Useful when just getting familiar with a library and type signatures
# and typed holes are all you have
./tidy-mklens --gen-type-alias-isos snapshots/UseTypeAliases.purs

# If the same label is used in multiple types,
# a lens for that label will be stored in each file,
# thereby duplicating the lens. This can lead to
# import frency.

./tidy-mklens snapshots/UseLocalProps/

# One way around this is to generate a single file
# that stores all lenses for the deduplicated labels,
# ensuring that label lenses are only imported from
# one place.
./tidy-mklens \
  --global-record-lens-overwrite-file \
  --global-record-lens-file snapshots/UseGlobalPropFile/GlobalRecordLens.purs \
  --global-record-lens-module Snapshots.UseGlobalPropFile.GlobalRecordLens \
  snapshots/UseGlobalPropFile

# Here's the primary example, showing the full power of the code
./tidy-mklens --gen-type-alias-isos snapshots/PrimaryExample.purs

# Open imports aren't always handled correctly. See these snapshots
./tidy-mklens snapshots/CheckOpenImports
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
