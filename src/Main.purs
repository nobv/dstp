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
  liftEffect $ foreach browser doJob j
  P.close browser

foreach :: forall p a. p -> (p -> a -> Effect Unit) -> Array a -> Effect Unit
foreach p f a
  | null a = Console.log "skip"
  | otherwise = do
    Console.log "stat"
    foreachE a $ f p

doJob :: P.Browser -> Job -> Effect Unit
doJob b j = do
  if j.enabled
     then launchAff_ do
          page <- P.newPage b
          P.goto page j.baseUrl
          liftEffect $ foreach page doStep j.steps
     else
          Console.log "skip this job"
          -- pure unit

doStep :: P.Page -> Command -> Effect Unit
doStep p c = do
  case c of
    Goto cmd -> Console.log "goto"
    SetInput cmd -> Console.log "input"
    Click cmd -> Console.log "click"
    Screenshot cmd -> Console.log "screenshot"
    WaitForSelector cmd -> Console.log "selector"
    WaitForNavigation cmd -> Console.log "navigation"


loadConfig :: String -> Effect (Maybe Config)
loadConfig config = do
  Y.parseYaml config
