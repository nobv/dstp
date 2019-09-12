module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Yaml as Y
import Dstp.FS as FS
import Dstp.Puppeteer as P
import Dstp.Types (Settings, Options)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
-- import Foreign.Generic (encodeJSON)
-- import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = do
  yamlStr <- FS.readFile "./config/config.yaml"
  config <- loadConfig yamlStr
  -- Console.log $ unsafeCoerce config
  case config of
    Nothing -> Console.log "nothing"
    Just c -> do
      -- Console.log $ encodeJSON c.jobs
      case c.options of
        Nothing -> Console.log "not found options"
        Just o -> do
          launch o

loadConfig :: String -> Effect (Maybe Settings)
loadConfig config = do
  Y.parseYaml config

launch :: Options -> Effect Unit
launch options = launchAff_ do
  browser <- P.launch options
  page <- P.newPage browser
  P.goto page "https://google.com"
  P.close browser
