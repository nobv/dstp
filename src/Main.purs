module Main where

import Data.Foldable
import Prelude

import Data.Array (null)
import Data.Maybe (Maybe(..))
import Data.Yaml as Y
import Dstp.FS as FS
import Dstp.Puppeteer as P
import Dstp.Types (Command(..), Config, Job, Options)
import Effect (Effect, foreachE)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console

-- import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = do
  yamlStr <- FS.readFile "./example/example.yaml"
  config <- loadConfig yamlStr
  case config of
    Nothing -> Console.log "nothing"
    Just c -> do
      case c.options of
        Nothing -> Console.log "not found options"
        Just o -> do
          case c.jobs of
            Nothing -> Console.log "not found jobs"
            Just j -> do
              launch o j

launch :: Options -> Array Job -> Effect Unit
launch o j = launchAff_ do
  browser <- P.launch o
  liftEffect $ foreach browser doJob j
  -- P.close browser

foreach :: forall p a. p -> (p -> a -> Effect Unit) -> Array a -> Effect Unit
foreach p f a
  | null a = Console.log "skip"
  | otherwise = do
    Console.log "start"
    foreachE a $ f p

doJob :: P.Browser -> Job -> Effect Unit
doJob b j = do
  if j.enabled
     then launchAff_ do
          page <- P.newPage b
          P.goto page j.baseUrl
          liftEffect $ foreach page (doStep j.baseUrl) j.steps
     else
          Console.log "skip this job"
          -- pure unit


doStep :: String -> P.Page -> Command -> Effect Unit
doStep s p c = launchAff_ do
  case c of
    Goto cmd -> do
      P.goto p $ s <> cmd.url
    SetInput cmd -> do
      P.setInput p cmd.selector cmd.value { delay: 0 }
    Click cmd -> do
      P.click p cmd.selector
    Screenshot cmd -> do
      P.screenshot p cmd
    WaitForSelector cmd -> do
      P.waitForSelector p cmd.selector
    WaitForNavigation cmd -> do
      P.waitForNavigation p

    Submit cmd -> do
      P.submit p cmd.selector

loadConfig :: String -> Effect (Maybe Config)
loadConfig config = do
  Y.parseYaml config
