module Hilt.Server where

import Control.Concurrent (forkIO)
import Data.ByteString (ByteString)

import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types                (status404)
import Network.Wai.Handler.WebSockets    (websocketsOr)
import Network.WebSockets                (defaultConnectionOptions)
import System.FilePath                   ((</>))
import Network.Wai                       (Middleware, responseLBS)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Cors       (CorsResourcePolicy(..), cors)
import Network.Wai.Middleware.HttpAuth   (basicAuth)
import Network.Wai.Middleware.Static     (staticPolicy, policy, Policy)
import Network.Wai.Middleware.Gzip       (gzip, def)
import qualified Network.Wai.Middleware.ForceSSL as M (forceSSL)


import qualified Hilt.Config as Config
import qualified Hilt.SocketServer as SocketServer


{- Fork a thread and boot the socket server as a Wai app on Warp -}
runWebsocket :: SocketServer.Handle -> IO ()
runWebsocket socketHandle = do
  port <- Config.lookupEnv "PORT" 8081

  -- @TODO serve static asset middleware
  let backupApp _ respond = respond $ responseLBS status404 [] "Not found."
      waiApp      = websocketsOr defaultConnectionOptions (SocketServer.app socketHandle) backupApp
      middlewares = staticFiles "public"

  _ <- forkIO $ Warp.run port $ middlewares waiApp

  return ()

-- runHttp @TODO
-- runWebsocketAndHttp @TODO



-- Middlewares
--
-- The following functions can be composed together and wrapped around a Wai app to
-- add specific middleware behavior. For example, a common full stack might look like;
--
--    middlewares = compression . staticFiles "public" . allowCsrf . corsified
--    runApp      = run port $ middlewares httpApp


-- | Basic HTTP Auth
-- The following header will be set: @Access-Control-Allow-Headers: x-csrf-token@.
auth :: ByteString -> ByteString -> Middleware
auth username password = basicAuth (\u p -> return $ u == username && p == password) "Authentication"


-- | @x-csrf-token@ allowance.
-- The following header will be set: @Access-Control-Allow-Headers: x-csrf-token@.
allowCsrf :: Middleware
allowCsrf = addHeaders [("Access-Control-Allow-Headers", "x-csrf-token,authorization")]

-- | CORS middleware configured with 'appCorsResourcePolicy'.
corsified :: Middleware
corsified = cors (const $ Just appCorsResourcePolicy)

-- | Adds static files directory, i.e. `staticFiles "public"` to serve from public folder
-- | Uses `index.html` as the index file on directory listing, i.e. `public/`
staticFiles :: String -> Middleware
staticFiles path = staticPolicy $ addBaseWithIndex path "index.html"

-- | Middleware to route static files and look for a default on index
addBaseWithIndex :: String -> String -> Policy
addBaseWithIndex base fallback = policy
  ( \req -> case req of
    "" -> Just (base </> fallback)
    _  -> Just (base </> req)
  )

compression :: Middleware
compression = gzip def

forceSSL :: Middleware
forceSSL = M.forceSSL

-- | Cors resource policy to be used with 'corsified' middleware.
--
-- This policy will set the following:
--
-- * RequestHeaders: @Content-Type@
-- * MethodsAllowed: @OPTIONS, GET, PUT, POST@
appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy = CorsResourcePolicy
  { corsOrigins        = Nothing
  , corsMethods        = ["OPTIONS", "GET", "PUT", "POST"]
  , corsRequestHeaders = ["Authorization", "Content-Type"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
  }
