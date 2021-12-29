# purescript-tidy-codegen-lens

Generate lenses and prisms for your data type automatically.

An input file, `./Foo.purs`, will generate an output file `./Foo/Lens.purs` if `Foo.purs` contains any type declarations.

This is currently a WIP.

## Usage

Run `spago build` and then `tidy-mklens <CLI args>`

See the [snapshots folder](./snapshots) for current output. Files were generated using the below commands:

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
  --global-record-lens-module Snapshots.GlobalRecordLens \
  snapshots/UseGlobalPropFile
```

## Roadmap

- update generated files to import non-Prim types based on the types imported in original file
