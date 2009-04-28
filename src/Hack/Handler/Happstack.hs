{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}

module Hack.Handler.Happstack (run) where

import Hack as Hack
import Happstack.Server.SimpleHTTP as Happstack

import Control.Arrow ((>>>))
import Data.Default
import Control.Monad.Trans
import Data.List
import Data.Char
import Data.Maybe

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
          , server_name = "" -- fst $ rqPeer req
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
headerToPair (HeaderPair k v) = (translate_header $ b2s k, intercalate " " $ map b2s v)
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
  

-- happstack convert all request header to lowercase ...

translate_header :: String -> String
translate_header s = fromMaybe s $ find (map toLower >>> (== s) ) header_list

header_list :: [String]
header_list = 
  [    "Cache-Control"        
  ,    "Connection"           
  ,    "Date"                 
  ,    "Pragma"               
  ,    "Transfer-Encoding"    
  ,    "Upgrade"              
  ,    "Via"                  
  ,    "Accept"               
  ,    "Accept-Charset"       
  ,    "Accept-Encoding"      
  ,    "Accept-Language"      
  ,    "Authorization"        
  ,    "Cookie"               
  ,    "Expect"               
  ,    "From"                 
  ,    "Host"                 
  ,    "If-Modified-Since"    
  ,    "If-Match"             
  ,    "If-None-Match"        
  ,    "If-Range"             
  ,    "If-Unmodified-Since"  
  ,    "Max-Forwards"         
  ,    "Proxy-Authorization"  
  ,    "Range"                
  ,    "Referer"              
  ,    "User-Agent"           
  ,    "Age"                  
  ,    "Location"             
  ,    "Proxy-Authenticate"   
  ,    "Public"               
  ,    "Retry-After"          
  ,    "Server"               
  ,    "Set-Cookie"           
  ,    "TE"                   
  ,    "Trailer"              
  ,    "Vary"                 
  ,    "Warning"              
  ,    "WWW-Authenticate"     
  ,    "Allow"                
  ,    "Content-Base"         
  ,    "Content-Encoding"     
  ,    "Content-Language"     
  ,    "Content-Length"       
  ,    "Content-Location"     
  ,    "Content-MD5"          
  ,    "Content-Range"        
  ,    "Content-Type"         
  ,    "ETag"                 
  ,    "Expires"              
  ,    "Last-Modified"        
  ,    "Content-Transfer-Encodeing"
  ]