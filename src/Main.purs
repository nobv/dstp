module Main where

import Data.Foldable
import Prelude

import Data.Array (null)
import Data.Array.Partial (tail)
import Data.Maybe (Maybe(..))
import Data.Yaml as Y
import Dstp.FS as FS
import Dstp.Puppeteer as P
import Dstp.Types (Command(..), Config, Options, Job)
import Effect (Effect, foreachE)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Generic (encodeJSON)
import Partial.Unsafe (unsafePartial)

-- import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = do
  yamlStr <- FS.readFile "./config/config.yaml"
  config <- loadConfig yamlStr
  -- Console.log $ unsafeCoerce config
  case config of
    Nothing -> Console.log "nothing"
    Just c -> do
      case c.options of
        Nothing -> Console.log "not found options"
        Just o -> do
          launch o

      case c.jobs of
        Nothing -> Console.log "not found jobs"
        Just j -> do
          doJobs j

doJobs :: Array Job -> Effect Unit
doJobs jx
  | null jx = Console.log "skip jobs"
  | otherwise = do
                Console.log "start jobs"
                foreachE jx doJob


doJob :: Job -> Effect Unit
doJob j = do
  Console.log j.name
  Console.log j.baseUrl
  Console.logShow j.enabled
  doSteps j.steps


doSteps :: Array Command -> Effect Unit
doSteps cs
  | null cs = Console.log "skip steps"
  | otherwise = do
                Console.log "stat steps"
                foreachE cs doStep

doStep :: Command -> Effect Unit
doStep c = do
  case c of
    Goto g -> Console.logShow g
    SetInput s -> Console.logShow s
    Click c -> Console.logShow c
    Screenshot ss -> Console.logShow ss
    WaitForSelector w -> Console.logShow w


loadConfig :: String -> Effect (Maybe Config)
loadConfig config = do
  Y.parseYaml config


launch :: Options -> Effect Unit
launch options = launchAff_ do
  browser <- P.launch options
  page <- P.newPage browser
  P.goto page "https://google.com"
  P.close browser
