module Main where

import Hack
import Hack.Handler.Hyena
import Hack.Contrib.Middleware.Inspect
import Hack.Contrib.Middleware.Debug
import Hack.Contrib.Utils
import Data.ByteString.Lazy.Char8 (pack)

app :: Application
app = \env -> return $
  Response 200 [ ("Content-Type", "text/plain") ] (pack "Hello World")

main = run $ use [debug (\e r -> print e), inspect] app