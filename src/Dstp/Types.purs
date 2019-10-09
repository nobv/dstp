module Dstp.Types where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe, fromMaybe)
import Data.Symbol (SProxy(..))
import Foreign.Generic (class Decode, class Encode, ForeignError(..), decode, encode)
import Record as Record


type Config =
  { options :: Maybe Options
  , jobs :: Maybe (Array Job)
  }

type Options =
  { headless :: Maybe Boolean
  , slowMo :: Maybe Int
  }

type Job =
  { name :: String
  , baseUrl :: String
  , enabled :: Boolean
  , steps :: Array Command
  }

data Command
  = Goto
      { url :: String
      , name :: Maybe String
      }
  | SetInput
      { selector :: String
      , value    :: String
      , name :: Maybe String
      , options :: Maybe InputOptions
      }
  | Click
      { selector :: String
      , name :: Maybe String
      }
  | Screenshot ScreenshotOptions
  | WaitForSelector
      { selector :: String
      , name :: Maybe String
      }
  | WaitForNavigation
      {}
  | Submit
      { name :: Maybe String
      , selector :: String
      }

type ScreenshotOptions =
  { path :: String
  , extension :: String
  , quality :: Number
  , fullPage :: Boolean
  , omitBackground :: Boolean
  , encoding :: String
  }

type InputOptions =
  { delay :: Int
  }

instance decodeCommand :: Decode Command where
  decode cmd = do
    { command } <- decode cmd :: _ { command :: String }
    case command of
      "Goto" -> Goto <$> decode cmd
      "SetInput" -> SetInput <$> decode cmd
      "Click" -> Click <$> decode cmd
      "Screenshot" -> map Screenshot $
        decode cmd <#> \opts -> opts
          { quality = fromMaybe 0.8 opts.quality
          , fullPage = fromMaybe true opts.fullPage
          , omitBackground = fromMaybe false opts.omitBackground
          , encoding = fromMaybe "binary" opts.encoding
          }
      "WaitForSelector" -> WaitForSelector <$> decode cmd
      "WaitForNavigation" -> WaitForNavigation <$> decode cmd
      "Submit" -> Submit <$> decode cmd
      _ -> throwError $ pure $ ForeignError $ "Unrecognised command: " <> command

instance encodeCommand :: Encode Command where
  encode (Goto opts) = encode $ Record.insert (SProxy :: _ "command") "Goto" opts
  encode (SetInput opts) = encode $ Record.insert (SProxy :: _ "command") "SetInput" opts
  encode (Click opts) = encode $ Record.insert (SProxy :: _ "command") "Click" opts
  encode (Screenshot opts) = encode $ Record.insert (SProxy :: _ "command") "Screenshot" opts
  encode (WaitForSelector opts) = encode $ Record.insert (SProxy :: _ "command") "WaitForSelector" opts
  encode (WaitForNavigation opts) = encode $ Record.insert (SProxy :: _ "command") "WaitForNavigation" opts
  encode (Submit opts) = encode $ Record.insert (SProxy :: _ "command") "Submit" opts
