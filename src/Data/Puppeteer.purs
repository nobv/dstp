module Data.Puppeteer where

import Prelude

import Control.Promise (Promise)
import Data.Function.Uncurried as FU
import Effect (Effect)

foreign import data Puppeteer :: Type
foreign import data Browser :: Type

foreign import puppeteer :: Puppeteer
foreign import callLaunch :: forall a. a -> Effect (Promise Browser)

type LaunchOptions =
  ( headless :: Boolean
  )


launch :: forall a. a -> Effect (Promise Browser)
launch options = callLaunch options
