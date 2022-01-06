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

./tidy-mklens.js --label-style-arg --output-dir snapshots snapshots/UseArgLabelStyle.purs

./tidy-mklens.js --label-style-abc --output-dir snapshots snapshots/UseAbcLabelStyle.purs

./tidy-mklens.js --gen-type-alias-isos --output-dir snapshots snapshots/UseTypeAliases.purs

./tidy-mklens.js --label-prefix "prop" --output-dir snapshots snapshots/UseLabelPrefix.purs

./tidy-mklens.js --label-prefix-none --output-dir snapshots snapshots/UseNoLabelPrefix.purs

./tidy-mklens.js --output-dir snapshots snapshots/UseLocalProps/

./tidy-mklens.js \
  --output-dir snapshots \
  --global-record-lens-module UseGlobalPropFile.GlobalRecordLens \
  snapshots/UseGlobalPropFile

./tidy-mklens.js --gen-type-alias-isos  --output-dir snapshots snapshots/PrimaryExample.purs

./tidy-mklens.js  --output-dir snapshots snapshots/CheckOpenImports