{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "assert"
  , "bigints"
  , "catenable-lists"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "folds"
  , "integers"
  , "lists"
  , "memoize"
  , "node-fs"
  , "node-fs-aff"
  , "node-process"
  , "node-streams"
  , "ordered-collections"
  , "pairs"
  , "parsing"
  , "pipes"
  , "psci-support"
  , "read"
  , "refs"
  , "st"
  , "string-parsers"
  , "strings"
  , "tailrec"
  , "these"
  , "transformers"
  , "tuples"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
