{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Hack.Handler.Hyena (run, runWithConfig, ServerConf(..)) where

import qualified Hack as Hack
import Hyena.Server
import Network.Wai as Wai


import Prelude hiding ((.), (^))
import System.IO
import Control.Monad

import Data.Default
import Data.Maybe
import Data.Char
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan,writeChan,getChanContents)

(.) :: a -> (a -> b) -> b
a . f = f a
infixl 9 .

-- | this won't change the port or servername for hyena, use
--   ./main -p 3000 to configure port
--   this just make sure port and serverName are presented in Env
data ServerConf = ServerConf { port :: Int, serverName :: String }
instance Default ServerConf where
  def = ServerConf { port = 3000, serverName = "localhost" }

to_s :: S.ByteString -> String
to_s = C.unpack

to_b :: String -> S.ByteString
to_b = C.pack

both_to_s :: (S.ByteString, S.ByteString) -> (String, String)
both_to_s (x,y) = (to_s x, to_s y)

both_to_b :: (String, String) -> (S.ByteString, S.ByteString)
both_to_b (x,y) = (to_b x, to_b y)

hyena_env_to_hack_env :: ServerConf -> Environment -> IO Hack.Env
hyena_env_to_hack_env conf e = do
  -- i <- (L.fromChunks <$> e.Wai.input .enumToList)
  return def
    {   
       Hack.requestMethod = convertRequestMethod (e.requestMethod)
    ,  Hack.scriptName    = e.scriptName.to_s
    ,  Hack.pathInfo      = e.pathInfo.to_s
    ,  Hack.queryString   = e.queryString .fromMaybe (to_b "") .to_s
    ,  Hack.http          = e.Wai.headers .map both_to_s
    ,  Hack.hackErrors    = e.errors
    ,  Hack.serverPort    = conf.port
    ,  Hack.serverName    = conf.serverName
    -- ,  Hack.hackInput     = i
    }
  where
    convertRequestMethod Wai.Options     =     Hack.OPTIONS
    convertRequestMethod Wai.Get         =     Hack.GET
    convertRequestMethod Wai.Head        =     Hack.HEAD
    convertRequestMethod Wai.Post        =     Hack.POST
    convertRequestMethod Wai.Put         =     Hack.PUT
    convertRequestMethod Wai.Delete      =     Hack.DELETE
    convertRequestMethod Wai.Trace       =     Hack.TRACE
    convertRequestMethod Wai.Connect     =     Hack.CONNECT
    
    enumToList enum = do 
      ch <- newChan
      _ <- forkIO $ enum (writer ch) ()
      getChanContents ch
      where
        writer ch () chunk = do
          writeChan ch chunk
          return (Right ())

enum_string :: L.ByteString -> IO Enumerator
enum_string msg = do
  let s = msg.L.unpack.to_b
  let yieldBlock f z = do
         z' <- f z s
         case z' of
           Left z''  -> return z''
           Right z'' -> return z''

  return yieldBlock

type WaiResponse = (Int, S.ByteString, Wai.Headers, Enumerator)

hack_response_to_hyena_response :: Enumerator -> Hack.Response -> WaiResponse
hack_response_to_hyena_response e r =
    (   r.Hack.status
    ,   r.Hack.status.show_status_message.fromMaybe "OK" .to_b
    ,   r.Hack.headers.map both_to_b
    ,   e
    )


hack_to_wai_with_config :: ServerConf -> Hack.Application -> Wai.Application
hack_to_wai_with_config conf app env = do
  hack_env <- env.hyena_env_to_hack_env conf
  
  r <- app hack_env
  
  enum <- r.Hack.body.enum_string
  let hyena_response = r.hack_response_to_hyena_response enum
  
  return hyena_response

run :: Hack.Application -> IO ()
run app = runWithConfig def app

runWithConfig :: ServerConf -> Hack.Application -> IO ()
runWithConfig conf app = app.hack_to_wai_with_config conf .serve


show_status_message :: Int -> Maybe String
show_status_message x = status_code.M.lookup x


status_code :: M.Map Int String
status_code =
  [  x       100          "Continue"
  ,  x       101          "Switching Protocols"
  ,  x       200          "OK"
  ,  x       201          "Created"
  ,  x       202          "Accepted"
  ,  x       203          "Non-Authoritative Information"
  ,  x       204          "No Content"
  ,  x       205          "Reset Content"
  ,  x       206          "Partial Content"
  ,  x       300          "Multiple Choices"
  ,  x       301          "Moved Permanently"
  ,  x       302          "Found"
  ,  x       303          "See Other"
  ,  x       304          "Not Modified"
  ,  x       305          "Use Proxy"
  ,  x       307          "Temporary Redirect"
  ,  x       400          "Bad Request"
  ,  x       401          "Unauthorized"
  ,  x       402          "Payment Required"
  ,  x       403          "Forbidden"
  ,  x       404          "Not Found"
  ,  x       405          "Method Not Allowed"
  ,  x       406          "Not Acceptable"
  ,  x       407          "Proxy Authentication Required"
  ,  x       408          "Request Timeout"
  ,  x       409          "Conflict"
  ,  x       410          "Gone"
  ,  x       411          "Length Required"
  ,  x       412          "Precondition Failed"
  ,  x       413          "Request Entity Too Large"
  ,  x       414          "Request-URI Too Large"
  ,  x       415          "Unsupported Media Type"
  ,  x       416          "Requested Range Not Satisfiable"
  ,  x       417          "Expectation Failed"
  ,  x       500          "Internal Server Error"
  ,  x       501          "Not Implemented"
  ,  x       502          "Bad Gateway"
  ,  x       503          "Service Unavailable"
  ,  x       504          "Gateway Timeout"
  ,  x       505          "HTTP Version Not Supported"
  ] .M.fromList
  where x a b = (a, b)