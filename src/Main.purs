module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Yaml as Y
import Dstp.FS as FS
import Dstp.Types (Settings)
import Effect (Effect)
import Effect.Class.Console as Console
import Foreign.Generic (encodeJSON)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = do
  yamlStr <- FS.readFile "./config/config.yaml"
  config <- loadConfig yamlStr
  Console.log $ unsafeCoerce config
  case config of
    Nothing -> Console.log "nothing"
    Just c -> do
      Console.log $ encodeJSON c.jobs

loadConfig :: String -> Effect (Maybe Settings)
loadConfig config = do
  Y.parseYaml config

-- launch :: Dstp.Settings -> Effect Unit
-- launch options = launchAff_ do
--   P.launch options


--puppeteer :: Effect Unit
--puppeteer = launchAff_ do
--  browser <- P.launch { headless: false
--                      , sloMo: 250
--                      }
--  page <- P.newPage browser
--  P.goto page "https://www.amazon.co.jp"
--  --P.click page "#nav-orders"
--  P.screenshot page { path: "test.png"
--                    , fullPage: true
--                    }
--  P.close browser
