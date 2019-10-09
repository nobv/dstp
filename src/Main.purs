module Main where

import Prelude

import Data.Array (null, head)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Yaml as Y
import Dstp.FS as FS
import Dstp.Puppeteer as P
import Dstp.Types (Command(..), Config, Job, Options)
import Effect (Effect)
import Effect.Aff (launchAff_, Aff)
import Effect.Class.Console as Console
import Node.Yargs.Applicative (flag, yarg, runY)
import Node.Yargs.Setup (example, usage)


main :: Effect Unit
main = do
  let setup = usage "$0 -f path/to/your/config.yaml"
              <> example "$0 -f ./example/example.yaml" ""

  runY setup $ test <$> yarg "f" ["file"] (Just "Path to your configration") (Right "configration path is required") false
                   <*> flag "h" ["headless"] (Just "Whether execute by headless mode or not")
                   <*> yarg "s" ["sloMo"] (Just "Slows down by the milliseconds") (Left 0) false


test :: Array String -> Boolean -> Int -> Effect Unit
test [] _ _ = Console.log "empty"
test xs a b = do
  for_ xs \n -> do
    Console.log $ "headless:" <> show a
    Console.log $ "sloMo:" <> show b
    Console.log n
  main' $ fromMaybe "" (head xs)

main' :: String -> Effect Unit
main' path = do
  yamlStr <- FS.readFile path
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
