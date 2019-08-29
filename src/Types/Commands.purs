module Types.Commands where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Foreign.Generic (defaultOptions, genericDecode)
import Foreign.Generic.Class (class Decode)

newtype Commands = Commands
  { goto :: Maybe Goto
  , input :: Maybe Input
  , click :: Maybe Click
  , screenshot :: Maybe Screenshot
  , waitForNavigation :: Maybe Unit
  , waitForSelector :: Maybe WaitForSelector
  }

newtype Goto = Goto
  { path :: String
  }

newtype Input = Input
  { fields :: Array Field
  }

newtype Field = Field
  { selector :: String
  , value    :: String
  }

newtype Click = Click
  { selector :: String
  }

newtype Screenshot = Screenshot
  { path :: Maybe String
  , extension :: Maybe String
  , quality :: Maybe Number
  , fullPage :: Maybe Boolean
  , clip :: Maybe Clip
  , omitBackground :: Maybe Boolean
  , encoding :: Maybe String
  }

newtype Clip = Clip
  { x :: Maybe Number
  , y :: Maybe Number
  , width  :: Maybe Number
  , height :: Maybe Number
  }

newtype WaitForSelector = WaitForSelector
  { selector :: Maybe String
  }


derive instance genericRoutes :: Generic Commands _
derive instance genericGoto :: Generic Goto _
derive instance genericInput :: Generic Input _
derive instance genericField :: Generic Field _
derive instance genericClick :: Generic Click _
derive instance genericScreenshot :: Generic Screenshot _
derive instance genericClip :: Generic Clip _
derive instance genericWfs :: Generic WaitForSelector _

instance showRoutes :: Show Commands where
  show = genericShow

instance showGoto :: Show Goto where
 show = genericShow

instance showInput :: Show Input where
 show = genericShow

instance showField :: Show Field where
 show = genericShow

instance showClick :: Show Click where
  show = genericShow

instance showScreenshot :: Show Screenshot where
  show = genericShow

instance showClip :: Show Clip where
  show = genericShow

instance shoewWaitForSelector :: Show WaitForSelector where
  show = genericShow

instance decodeRoutes :: Decode Commands where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance decodeGoto :: Decode Goto where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance decodeInput :: Decode Input where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance decodeField :: Decode Field where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance decodeClick :: Decode Click where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance decodeScreenshot :: Decode Screenshot where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance decodeClip :: Decode Clip where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance decodeWaitForSelector :: Decode WaitForSelector where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

