{ name = "snapshots"
, dependencies =
  [ "either"
  , "newtype"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "snapshots/**/*.purs" ]
}
