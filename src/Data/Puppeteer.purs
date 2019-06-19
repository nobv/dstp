module Data.Puppeteer where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried as FU
import Effect (Effect)
import Effect.Aff (Aff)

foreign import data Puppeteer :: Type
foreign import data Browser :: Type

foreign import puppeteer :: Puppeteer
foreign import callLaunch :: forall t1. t1
foreign import callClose :: forall t1. t1

type LaunchOptions =
  ( headless :: Boolean
  )

launch :: forall t8. t8 -> Aff Browser
launch = runPromiseAffE1 callLaunch

close :: forall t12 t13. t12 -> Aff t13
close = runPromiseAffE1 callClose

runPromiseAffE1 :: forall t5 t7. (t7 -> Effect (Promise t5)) -> t7 -> Aff t5
runPromiseAffE1 f a = Promise.toAffE $ FU.runFn1 f a

