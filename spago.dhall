{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "aoc2020"
, dependencies =
  [ "console"
  , "effect"
  , "generics-rep"
  , "node-buffer"
  , "node-fs"
  , "psci-support"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
