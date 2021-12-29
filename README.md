# purescript-tidy-codegen-lens

Generate lenses and prisms for your data types automatically.

An input file, `./Foo.purs`, will generate an output file `./Foo/Lens.purs` if `Foo.purs` contains any type declarations.

This is currently a WIP.

## Usage

Run `spago build` and then `./tidy-mklens <CLI args>`

The table below summarizes the current output stored in the [snapshots folder](./snapshots).

| Purpose | Files |
| - | - |
| Show the 'arg' label style | <ul><li><p>Source file</p><ul><li>[UseArgLabelStyle.purs](./snapshots/UseArgLabelStyle.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseArgLabelStyle/Lens.purs](./snapshots/UseArgLabelStyle/Lens.purs)</li></ul></li></ul> |
| Show the 'abc' label style | <ul><li><p>Source file</p><ul><li>[UseAbcLabelStyle.purs](./snapshots/UseAbcLabelStyle.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseAbcLabelStyle/Lens.purs](./snapshots/UseAbcLabelStyle/Lens.purs)</li></ul></li></ul> |
| Show the `--gen-type-alias-lenses` option | <ul><li><p>Source file</p><ul><li>[UseTypeAlises.purs](./snapshots/UseTypeAlises.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseTypeAlises/Lens.purs](./snapshots/UseTypeAlises/Lens.purs)</li></ul></li></ul> |
| Primary example<br /><br />The record label lenses (see the `SameLabelsAsOtherModule` type) will be duplicated in each module. This is why why the `--gen-record-lens-*` options exist | <ul><li><p>Source file</p><ul><li>[UseLocalProps/Module1.purs](./snapshots/UseLocalProps/Module1.purs)</li><li>[UseLocalProps/Module2.purs](./snapshots/UseLocalProps/Module2.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseLocalProps/Module1/Lens.purs](./snapshots/UseLocalProps/Module1/Lens.purs)</li><li>[UseLocalProps/Module2/Lens.purs](./snapshots/UseLocalProps/Module2/Lens.purs)</li></ul></li></ul> |
| Primary example<br /><br />Demo the `--gen-record-lens-*` options | <ul><li><p>Source file</p><ul><li>[UseGlobalPropFile/Module1.purs](./snapshots/UseGlobalPropFile/Module1.purs)</li><li>[UseGlobalPropFile/Module2.purs](./snapshots/UseGlobalPropFile/Module2.purs)</li></ul></li><li><p>Output file(s)</p><ul><li>[UseGlobalPropFile/Module1/Lens.purs](./snapshots/UseGlobalPropFile/Module1/Lens.purs)</li><li>[UseGlobalPropFile/Module2/Lens.purs](./snapshots/UseGlobalPropFile/Module2/Lens.purs)</li><li>[UseGlobalPropFile/GlobalRecordLens.purs](./snapshots/UseGlobalPropFile/GlobalRecordLens.purs)</li></ul></li></ul> |

Files were generated using the below commands, which are stored in [regen-snapshots.sh](./regen-snapshots.sh):

```bash
# Note: the `arg` style is the default
./tidy-mklens --label-style-arg snapshots/UseArgLabelStyle.purs

./tidy-mklens --label-style-abc snapshots/UseAbcLabelStyle.purs

# Can optionally generate lenses for type aliases
# Useful when just getting familiar with a library and type signatures
# and typed holes are all you have
./tidy-mklens --gen-type-alias-lenses snapshots/UseTypeAliases.purs

# The remaining two examples demo the full power of the code

./tidy-mklens snapshots/UseLocalProps/

# Notice how the previous one generated multiple label lenses
# for the same label? How about we deduplicate that?
./tidy-mklens \
  --global-record-lens-overwrite-file \
  --global-record-lens-file snapshots/UseGlobalPropFile/GlobalRecordLens.purs \
  --global-record-lens-module Snapshots.UseGlobalPropFile.GlobalRecordLens \
  snapshots/UseGlobalPropFile
```
