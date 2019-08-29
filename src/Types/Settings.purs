module Types.Settings where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign.Generic (defaultOptions, genericDecode)
import Foreign.Generic.Class (class Decode)
import Types.Task (Task)
import Types.Options (Options)

newtype Settings = Settings
  { options :: Maybe Options
  , tasks :: Maybe (Array Task)
  }

derive instance genericSettings :: Generic Settings _

instance showSetting :: Show Settings where
  show = genericShow

instance decodeSettings :: Decode Settings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
