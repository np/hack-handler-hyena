{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Hack.Handler.Hyena (run) where

import Hack as Hack
import Hack.Utils

import Hyena.Server
import Network.Wai as Wai

import Data.Default
import Prelude hiding ((.), (^))
import MPSUTF8 hiding (to_s)
import System.IO
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as S

import Control.Monad
import Data.Maybe

to_s :: S.ByteString -> String
to_s = C.unpack

to_b :: String -> S.ByteString
to_b = C.pack

both_to_s :: (S.ByteString, S.ByteString) -> (String, String)
both_to_s (a,b) = (a.to_s, b.to_s)

both_to_b :: (String, String) -> (S.ByteString, S.ByteString)
both_to_b (a,b) = (a.to_b, b.to_b)

hyena_env_to_hack_env :: Environment -> IO Hack.Env
hyena_env_to_hack_env e = return $
  def
    {   
       request_method = e.requestMethod.show.upper.read
    ,  script_name    = e.scriptName.to_s
    ,  path_info      = e.pathInfo.to_s
    ,  query_string   = e.queryString .fromMaybe (to_b "") .to_s
    ,  http           = e.Wai.headers .map both_to_s
    ,  hack_errors    = e.errors
    }

-- hack_response_to_hyena_response :: Hack.Response -> (Int, S.ByteString, Headers, Enumerator)

enum_string :: String -> IO Enumerator
enum_string msg = do
  let s = msg.to_b
  let yieldBlock f z = do
         z' <- f z s
         case z' of
           Left z''  -> return z''
           Right z'' -> return z''

  return yieldBlock

type WaiResponse = (Int, S.ByteString, Wai.Headers, Enumerator)

hack_response_to_hyena_response :: Enumerator -> Hack.Response -> WaiResponse
hack_response_to_hyena_response e r =
    (   r.status
    ,   r.status.show_status_code.fromMaybe "OK" .to_b
    ,   r.Hack.headers.map both_to_b
    ,   e
    )


hack_to_wai :: Hack.Application -> Wai.Application
hack_to_wai app env = do
  hack_env <- env.hyena_env_to_hack_env
  
  r <- app hack_env
  
  enum <- r.body.enum_string
  let hyena_response = r.hack_response_to_hyena_response enum
  
  return hyena_response

run :: Hack.Application -> IO ()
run app = app.hack_to_wai.serve
