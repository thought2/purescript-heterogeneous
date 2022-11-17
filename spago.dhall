{ name = "heterogeneous"
, dependencies =
  [ "bifunctors"
  , "either"
  , "foldable-traversable"
  , "functors"
  , "maybe"
  , "prelude"
  , "record"
  , "tuples"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}