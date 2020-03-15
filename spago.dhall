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
    , "dotenv"
    , "effect"
    , "foreign"
    , "foreign-generic"
    , "generics-rep"
    , "node-fs"
    , "node-readline"
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
