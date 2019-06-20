module Data.Puppeteer where

import Effect.Class (liftEffect)
import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)

foreign import data Puppeteer :: Type
foreign import data Browser :: Type
foreign import data Page :: Type

foreign import puppeteer :: Puppeteer
foreign import _launch :: forall options. options -> Effect (Promise Browser)
foreign import _newPage :: Browser -> Effect (Promise Page)
foreign import _goto :: Page -> String -> Effect (Promise Unit)
foreign import _close :: Browser -> Effect (Promise Unit)


type LaunchOptions =
  ( headless :: Boolean
  )


launch :: forall options. options -> Aff Browser
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
  promise <- liftEffect (_close browser)
  Promise.toAff promise
