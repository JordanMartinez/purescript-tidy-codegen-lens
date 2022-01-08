#!/usr/bin/env bash

rm -rf lenses

./tidy-mklens.js \
  --output-dir lenses \
  --gen-type-alias-isos \
  --label-prefix-none \
  --global-record-lens-module Dependencies.RecordLens \
  .spago/*/*/src/**/*.purs:4