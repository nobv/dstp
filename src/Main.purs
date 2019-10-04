module Main where

import Data.Foldable (for_)
import Prelude

import Data.Array (null)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Yaml as Y
import Dstp.FS as FS
import Dstp.Puppeteer as P
import Dstp.Types (Command(..), Config, Job, Options)
import Effect (Effect)
import Effect.Aff (launchAff_, Aff)
import Effect.Class.Console as Console

main :: Effect Unit
main = do
  yamlStr <- FS.readFile "./example/example.yaml"
  config <- loadConfig yamlStr
  case config of
    Nothing -> Console.log "not found config"
    Just c -> do
      case c.options of
        Nothing -> Console.log "not found options"
        Just o -> do
          case c.jobs of
            Nothing -> Console.log "not found jobs"
            Just j -> launchAff_  do
              launch o j

launch :: Options -> Array Job -> Aff Unit
launch o j = do
  browser <- P.launch o
  foreach browser doJob j
  P.close browser

foreach :: forall p a. p -> (p -> a -> Aff Unit) -> Array a -> Aff Unit
foreach page func array
  | null array = Console.log "skip"
  | otherwise = do
    Console.log "start"
    for_ array \n -> do
      func page n

doJob :: P.Browser -> Job -> Aff Unit
doJob b j = do
  if j.enabled
     then do
          page <- P.newPage b
          P.goto page j.baseUrl
          foreach page (doStep j.baseUrl) j.steps
     else
          Console.log "skip this job"
          -- pure unit

doStep :: String -> P.Page -> Command -> Aff Unit
doStep s p c = do
  case c of
    Goto cmd -> do
      P.goto p $ s <> cmd.url
    SetInput cmd -> do
      P.waitForSelector p cmd.selector
      P.setInput p cmd.selector cmd.value $ fromMaybe { delay: 0 } cmd.options
    Click cmd -> do
      P.waitForSelector p cmd.selector
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
