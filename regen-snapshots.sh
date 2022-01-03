#!/usr/bin/env bash

rm \
  snapshots/CheckOpenImports/Reimport/Lens.purs \
  snapshots/CheckOpenImports/Remove/Lens.purs \
  snapshots/CheckOpenImports/RemoveHiding/Lens.purs \
  snapshots/PrimaryExample/Lens.purs \
  snapshots/UseAbcLabelStyle/Lens.purs \
  snapshots/UseArgLabelStyle/Lens.purs \
  snapshots/UseGlobalPropFile/GlobalRecordLens.purs \
  snapshots/UseLocalProps/Module1/Lens.purs \
  snapshots/UseLocalProps/Module2/Lens.purs \
  snapshots/UseTypeAliases/Lens.purs \
  snapshots/UseLabelPrefix/Lens.purs \
  snapshots/UseNoLabelPrefix/Lens.purs

./tidy-mklens --label-style-arg snapshots/UseArgLabelStyle.purs

./tidy-mklens --label-style-abc snapshots/UseAbcLabelStyle.purs

./tidy-mklens --gen-type-alias-isos snapshots/UseTypeAliases.purs

./tidy-mklens --label-prefix "prop" snapshots/UseLabelPrefix.purs

./tidy-mklens --label-prefix-none snapshots/UseNoLabelPrefix.purs

./tidy-mklens snapshots/UseLocalProps/

./tidy-mklens \
  --global-record-lens-overwrite-file \
  --global-record-lens-file snapshots/UseGlobalPropFile/GlobalRecordLens.purs \
  --global-record-lens-module Snapshots.UseGlobalPropFile.GlobalRecordLens \
  snapshots/UseGlobalPropFile

./tidy-mklens --gen-type-alias-isos snapshots/PrimaryExample.purs

./tidy-mklens snapshots/CheckOpenImports
