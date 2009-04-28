{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}

module Hack.Handler.Happstack (run) where

import Hack as Hack
import Happstack.Server.SimpleHTTP as Happstack

import Control.Arrow ((>>>))
import Data.Default
import Control.Monad.Trans
import Data.List
import Data.Char

import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString as L
import qualified Data.ByteString.Internal as I

run_with_config :: Conf -> Hack.Application -> IO ()
run_with_config conf app =
 Happstack.simpleHTTP conf $ myPart conf app

run :: Hack.Application -> IO ()
run = run_with_config $ nullConf { port = 3000 }

myPart :: Conf -> Hack.Application -> ServerPart (Happstack.Response)
myPart conf app = do
  req <- Happstack.askRq
  let env = reqToEnv conf req
  resp <- liftIO $ app env
  return $ toHappstackResponse resp

  where
    reqToEnv conf' req =
      def { request_method = convertRequestMethod $ rqMethod req
          , script_name = ""
          , path_info = "/" ++ (intercalate "/" $ rqPaths req)
          , query_string = rqQuery req
          , server_name = fst $ rqPeer req
          , server_port = port conf'
          , http = toHttp (rqHeaders req)
          , hack_input = (\(Body x) -> C.unpack x) (rqBody req)
          }
    
    
    convertRequestMethod Happstack.OPTIONS     =     Hack.OPTIONS
    convertRequestMethod Happstack.GET         =     Hack.GET
    convertRequestMethod Happstack.HEAD        =     Hack.HEAD
    convertRequestMethod Happstack.POST        =     Hack.POST
    convertRequestMethod Happstack.PUT         =     Hack.PUT
    convertRequestMethod Happstack.DELETE      =     Hack.DELETE
    convertRequestMethod Happstack.TRACE       =     Hack.TRACE
    convertRequestMethod Happstack.CONNECT     =     Hack.CONNECT
          
          
          
toHttp :: Headers -> Hack.Map
toHttp = M.toList >>> map snd >>> map headerToPair

headerToPair :: HeaderPair -> (String, String)
headerToPair (HeaderPair k v) = (b2s k, intercalate " " $ map b2s v)
  where b2s x = map I.w2c $ L.unpack x

toHappstackResponse :: Hack.Response -> Happstack.Response
toHappstackResponse resp =
 Happstack.Response { rsCode = Hack.status resp
                    , rsHeaders = convertHeaders $ Hack.headers resp
                    , rsFlags = RsFlags {rsfContentLength = False}
                    , rsBody = C.pack $ Hack.body resp
                    , rsValidator = Nothing }


convertHeaders :: Hack.Map -> Happstack.Headers
convertHeaders = map pairToHeader >>> M.fromList
  where 
    pairToHeader (k,v) = ((c2b $ map toLower k), HeaderPair (c2b k) [c2b v])
    c2b x = L.pack $ map I.c2w x
  

