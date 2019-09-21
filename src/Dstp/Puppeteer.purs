module Dstp.Puppeteer where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (fromMaybe)
import Dstp.Types (Options, ScreenshotOptions, InputOptions)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, runEffectFn1, runEffectFn2, runEffectFn4)

--import Unsafe.Coerce (unsafeCoerce)


foreign import data Browser :: Type
foreign import data Page :: Type

foreign import launchImpl :: EffectFn1 OptionsDto (Promise Browser)
foreign import newPageImpl :: EffectFn1 Browser (Promise Page)
foreign import gotoImpl :: EffectFn2 Page String (Promise Unit)
foreign import closeImpl :: EffectFn1 Browser (Promise Unit)
foreign import clickImpl :: EffectFn2 Page Selector (Promise Unit)
foreign import screenshotImpl :: EffectFn2 Page ScreenshotOptions (Promise Unit)
foreign import submitImpl :: EffectFn2 Page Selector (Promise Unit)
foreign import waitForNavigationImpl :: EffectFn1 Page (Promise Unit)
foreign import waitForSelectorImpl :: EffectFn2 Page Selector (Promise Unit)
foreign import typeImpl :: EffectFn4 Page Selector String InputOptions (Promise Unit)

type Selector = String

type OptionsDto =
  { headless :: Boolean
  , sloMo :: Int
  }


launch :: Options -> Aff Browser
launch options = do
  Console.logShow options
  promise <- liftEffect (runEffectFn1 launchImpl { headless: fromMaybe true options.headless
                                                 , sloMo: fromMaybe 0 options.sloMo
                                                 })
  Promise.toAff promise


newPage :: Browser -> Aff Page
newPage browser = do
  Console.log "new page"
  promise <- liftEffect (runEffectFn1 newPageImpl browser)
  Promise.toAff promise

goto :: Page -> String -> Aff Unit
goto page url = do
  Console.log "goto"
  promise <- liftEffect (runEffectFn2 gotoImpl page url)
  Promise.toAff promise

close :: Browser -> Aff Unit
close browser = do
  Console.log "Closing browser"
  promise <- liftEffect (runEffectFn1 closeImpl browser)
  Promise.toAff promise

click :: Page -> Selector -> Aff Unit
click page selector = do
  Console.log "click"
  promise <- liftEffect (runEffectFn2 clickImpl page selector)
  Promise.toAff promise

screenshot :: Page -> ScreenshotOptions -> Aff Unit
screenshot page option = do
  Console.log "screenshot"
  promise <- liftEffect (runEffectFn2 screenshotImpl page option)
  Promise.toAff promise

submit :: Page -> Selector -> Aff Unit
submit page selector = do
  Console.log "submit"
  promise <- liftEffect (runEffectFn2 submitImpl page selector)
  Promise.toAff promise

waitForNavigation :: Page -> Aff Unit
waitForNavigation page = do
  Console.log "waitForNavigation"
  promise <- liftEffect (runEffectFn1 waitForNavigationImpl page)
  Promise.toAff promise

waitForSelector :: Page -> Selector -> Aff Unit
waitForSelector page selector = do
  Console.log "waitForSelector"
  promise <- liftEffect (runEffectFn2 waitForSelectorImpl page selector)
  Promise.toAff promise

setInput :: Page -> Selector -> String -> InputOptions -> Aff Unit
setInput page selector value opitons = do
  Console.log "setInput"
  _ <- waitForSelector page selector
  promise <- liftEffect (runEffectFn4 typeImpl page selector value opitons)
  Console.log "setInput after"
  Promise.toAff promise
