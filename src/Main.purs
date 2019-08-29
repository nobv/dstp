module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Yaml as Y
import Types.Dstp (Dstp)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Libs.Fs as F
import Libs.Puppeteer as P

main :: _
main = do
  yamlStr <- F.readFile "./config/config.yaml"
  config <- loadConfig yamlStr
  case config of
    Nothing -> Console.log "nothing"
    Just (c :: Dstp) -> do
      Console.logShow c.options


loadConfig :: String -> Effect (Maybe Dstp)
loadConfig config = do
  Y.parseYaml config

loadConfig' :: String -> Effect Unit
loadConfig' config = do
  maybeYaml <- Y.parseYaml config
  case maybeYaml of
    Nothing -> Console.log "can't load config file"
    Just (yaml :: Dstp) -> Console.log $ show yaml

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
