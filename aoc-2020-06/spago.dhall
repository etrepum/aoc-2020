{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "assert"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "newtype"
  , "node-fs"
  , "node-fs-aff"
  , "node-process"
  , "node-streams"
  , "ordered-collections"
  , "pairs"
  , "parsing"
  , "psci-support"
  , "record"
  , "refs"
  , "st"
  , "string-parsers"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
