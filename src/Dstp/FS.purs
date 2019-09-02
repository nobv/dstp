module Dstp.FS where

import Prelude

import Effect (Effect)
import Node.Buffer (Buffer)
import Node.Encoding (Encoding(..))
import Node.FS.Async as Async
import Node.FS.Sync as Sync

type Path = String

readFile :: Path -> Effect String
readFile path = Sync.readTextFile UTF8 path

readFileAsync :: Path -> Async.Callback Buffer -> Effect Unit
readFileAsync path callback = Async.readFile path callback
