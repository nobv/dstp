module Data.Puppeteer where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console

foreign import data Browser :: Type
foreign import data Page :: Type

foreign import _launch :: forall options. options -> Effect (Promise Browser)
foreign import _newPage :: Browser -> Effect (Promise Page)
foreign import _goto :: Page -> String -> Effect (Promise Unit)
foreign import _close :: Browser -> Effect (Promise Unit)


type Options =
  { headless :: Boolean
  }

launch :: Options -> Aff Browser
launch options = do
  promise <- liftEffect (_launch options)
  Promise.toAff promise


newPage :: Browser -> Aff Page
newPage browser = do
  promise <- liftEffect (_newPage browser)
  Promise.toAff promise

goto :: Page -> String -> Aff Unit
goto page url = do
  promise <- liftEffect (_goto page url)
  Promise.toAff promise

close :: Browser -> Aff Unit
close browser = do
  Console.log "Closing browser"
  promise <- liftEffect (_close browser)
  Promise.toAff promise
