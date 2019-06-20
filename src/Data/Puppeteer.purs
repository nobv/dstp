module Data.Puppeteer where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)

foreign import data Puppeteer :: Type
foreign import data Browser :: Type

foreign import puppeteer :: Puppeteer
foreign import callLaunch :: forall a. a -> Aff (Promise Browser)
foreign import callClose ::  Browser -> Effect (Promise Unit)

type LaunchOptions =
  ( headless :: Boolean
  )


launch :: forall a. a -> Aff Browser
launch options = callLaunch options >>= Promise.toAff

close :: Browser -> Effect (Promise Unit)
close = callClose
