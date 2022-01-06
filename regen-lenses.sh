#!/usr/bin/env bash

rm -rf lenses

./tidy-mklens.js \
  --output-dir lenses \
  --gen-type-alias-isos \
  --label-prefix-none \
  --global-record-lens-overwrite-file \
  --global-record-lens-file lenses/RecordLens.purs \
  --global-record-lens-module Dependencies.RecordLens \
  .spago/*/*/src/**/*.purs:4