module Main where

import Prelude

import Data.Puppeteer as P
import Effect (Effect)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = launchAff_ do
  browser <- P.launch { }
  page <- P.newPage browser
  P.goto page "https://google.com"
  P.close browser

