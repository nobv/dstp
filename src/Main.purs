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
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console
import Node.Yargs.Applicative (flag, yarg, runY)
import Node.Yargs.Setup (example, usage)

type Path = String

main :: Effect Unit
main = do
  let setup = usage "$0 -f path/to/your/config.yaml"
              <> example "$0 -f ./example/example.yaml" ""

  runY setup $ app <$> yarg "f" ["file"] (Just "Path to your configration") (Right "configration path is required") false
                   <*> flag "h" ["headless"] (Just "Whether execute by headless mode or not")
                   <*> yarg "s" ["sloMo"] (Just "Slows down by the milliseconds") (Left 0) false


app :: Array String -> Boolean -> Int -> Effect Unit
app [] _ _ = Console.log "empty"
app xs a b = do
  mkConfig (fromMaybe "" (head xs)) (mkOptions a b)

mkOptions :: Boolean -> Int -> Options
mkOptions headless slowMo = do
  { headless: Just headless
  , slowMo: Just slowMo
  }

mkConfig :: Path -> Options -> Effect Unit
mkConfig path options = do
  yamlStr <- FS.readFile path
  config <- loadConfig yamlStr
  case (config >>= _.jobs) of
    Nothing -> Console.log "config file or jobs section are required"
    Just c -> launchAff_ do
      launch options c

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
