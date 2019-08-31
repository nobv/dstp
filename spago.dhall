{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "dstp"
, dependencies =
    [ "aff"
    , "aff-promise"
    , "console"
    , "effect"
    , "foreign"
    , "foreign-generic"
    , "generics-rep"
    , "node-fs"
    , "prelude"
    , "psci-support"
    , "transformers"
    , "yargs"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
