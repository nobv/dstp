module Main where

import Data.Foldable
import Prelude

import Data.Array (null)
import Data.Array.Partial (tail)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Yaml as Y
import Dstp.FS as FS
import Dstp.Puppeteer as P
import Dstp.Types (Command(..), Config, Job, Options)
import Effect (Effect, foreachE)
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Generic (encodeJSON)
import Partial.Unsafe (unsafePartial)

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
  liftEffect $ doJobs browser j
  pure $ P.close browser

doJobs :: P.Browser -> Array Job -> Effect Unit
doJobs b jx
  | null jx = Console.log "skip jobs"
  | otherwise = do
                Console.log "start jobs"
                foreachE jx $ doJob b


doJob :: P.Browser -> Job -> Effect Unit
doJob b j = do
  if j.enabled
     then launchAff_ do
          Console.log $ encodeJSON j
          page <- P.newPage b
          P.goto page j.baseUrl
          Console.log "after goto"
          pure $ doSteps page j.steps
     else
          Console.log "unit"
          -- pure unit


doSteps :: P.Page -> Array Command -> Effect Unit
doSteps p cs
  | null cs = Console.log "skip steps"
  | otherwise = do
                Console.log "stat steps"
                foreachE cs $ doStep p

doStep :: P.Page -> Command -> Effect Unit
doStep p c = do
  case c of
    Goto cmd -> Console.logShow cmd
    SetInput cmd -> Console.logShow cmd
    Click cmd -> Console.logShow cmd
    Screenshot cmd -> Console.logShow cmd
    WaitForSelector cmd -> Console.logShow cmd


loadConfig :: String -> Effect (Maybe Config)
loadConfig config = do
  Y.parseYaml config
