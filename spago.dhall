{ name = "my-project"
, dependencies =
  [ "aff"
  , "argparse-basic"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "free"
  , "identity"
  , "language-cst-parser"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-glob-basic"
  , "node-path"
  , "node-process"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "psci-support"
  , "safe-coerce"
  , "strings"
  , "tidy-codegen"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
