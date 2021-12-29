# purescript-tidy-codegen-lens

Generate lenses and prisms for your data types automatically.

An input file, `./Foo.purs`, will generate an output file `./Foo/Lens.purs` if `Foo.purs` contains any type declarations.

This is currently a WIP.

## Usage

Run `spago build` and then `./tidy-mklens <CLI args>`

Read the table below and then look at the [snapshots folder](./snapshots) for current output.

| Purpose | Source Files | Outputted Files |
| - | - | - |
| Show the 'arg' label style | <ul><li>[UseArgLabelStyle.purs](./snapshots/UseArgLabelStyle.purs)</li></ul> | <ul><li>[UseArgLabelStyle/Lens.purs](./snapshots/UseArgLabelStyle/Lens.purs)</li></ul> |
| Show the 'abc' label style | <ul><li>[UseAbcLabelStyle.purs](./snapshots/UseAbcLabelStyle.purs)</li></ul> | <ul><li>[UseAbcLabelStyle/Lens.purs](./snapshots/UseAbcLabelStyle/Lens.purs)</li></ul> |
| Show the `--gen-type-alias-lenses` option | <ul><li>[UseTypeAlises.purs](./snapshots/UseTypeAlises.purs)</li></ul> | <ul><li>[UseTypeAlises/Lens.purs](./snapshots/UseTypeAlises/Lens.purs)</li></ul> |
| Primary example; Show why the `--gen-record-lens-*` options exist | <ul><li>[UseLocalProps/Module1.purs](./snapshots/UseLocalProps/Module1.purs)</li><li>[UseLocalProps/Module2.purs](./snapshots/UseLocalProps/Module2.purs)</li></ul> | <ul><li>[UseLocalProps/Module1/Lens.purs](./snapshots/UseLocalProps/Module1/Lens.purs)</li><li>[UseLocalProps/Module2/Lens.purs](./snapshots/UseLocalProps/Module2/Lens.purs)</li></ul> |
| Primary example; demo the `--gen-record-lens-*` options | <ul><li>[UseGlobalPropFile/Module1.purs](./snapshots/UseGlobalPropFile/Module1.purs)</li><li>[UseGlobalPropFile/Module2.purs](./snapshots/UseGlobalPropFile/Module2.purs)</li></ul> | <ul><li>[UseGlobalPropFile/Module1/Lens.purs](./snapshots/UseGlobalPropFile/Module1/Lens.purs)</li><li>[UseGlobalPropFile/Module2/Lens.purs](./snapshots/UseLocalProps/Module2/Lens.purs)</li><li>[UseGlobalPropFile/GlobalRecordLens.purs](./snapshots/UseGlobalPropFile/GlobalRecordLens.purs)</li></ul> |

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
