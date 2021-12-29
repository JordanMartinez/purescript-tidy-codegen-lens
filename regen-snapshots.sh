#!/usr/bin/env bash

./tidy-mklens --label-style-arg snapshots/UseArgLabelStyle.purs

./tidy-mklens --label-style-abc snapshots/UseAbcLabelStyle.purs

./tidy-mklens --gen-type-alias-lenses snapshots/UseTypeAliases.purs

./tidy-mklens snapshots/UseLocalProps/

./tidy-mklens \
  --global-record-lens-overwrite-file \
  --global-record-lens-file snapshots/UseGlobalPropFile/GlobalRecordLens.purs \
  --global-record-lens-module Snapshots.UseGlobalPropFile.GlobalRecordLens \
  snapshots/UseGlobalPropFile

./tidy-mklens snapshots/PrimaryExample.purs
