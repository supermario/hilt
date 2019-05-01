module Hilt.Server (module Hilt.Server, addHeaders) where

import Control.Concurrent (forkIO)
import Data.ByteString (ByteString)
import Data.Function ((&))

import qualified Network.Wai.Handler.Warp as W
import Network.HTTP.Types                (status404, status200)
import Network.WebSockets                (ServerApp, defaultConnectionOptions)
import System.FilePath                   ((</>))
import Network.Wai                       (Application, Middleware, responseLBS, responseFile)
import Network.Wai.Handler.WebSockets    (websocketsOr)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Cors       (CorsResourcePolicy(..), cors)
import Network.Wai.Middleware.HttpAuth   (basicAuth)
import Network.Wai.Middleware.Static     (staticPolicy, policy, Policy)
import Network.Wai.Middleware.Gzip       (gzip, def)
import qualified Network.Wai.Middleware.ForceSSL as M (forceSSL)
import System.IO                         (FilePath)

import qualified Hilt.Config as Config
import qualified Hilt.SocketServer as SocketServer


type Middlewares = (Network.Wai.Application -> Network.Wai.Application)

{- Fork a thread and boot the http server as a Wai app on Warp -}
runHttp :: Network.Wai.Application -> Middlewares -> IO ()
runHttp = boot


{- Fork a thread and boot the websocket server as a Wai app on Warp -}
runWebsocket :: SocketServer.Handle -> Middlewares -> IO ()
runWebsocket socketHandle middlewares = do
  let backupApp _ respond = respond $ responseLBS status404 [] "Not found."
      waiApp = websocketsOr defaultConnectionOptions (SocketServer.app socketHandle) backupApp

  boot waiApp middlewares


{- Fork a thread and boot the websocket server as a Wai app on Warp, with a filepath fallback for 404s -}
runWebsocketWithFallback :: SocketServer.Handle -> Middlewares -> FilePath -> IO ()
runWebsocketWithFallback socketHandle middlewares filepath = do
  let backupApp _ respond = respond $ responseFile status200 [] filepath Nothing
      waiApp = websocketsOr defaultConnectionOptions (SocketServer.app socketHandle) backupApp

  boot waiApp middlewares


{- Fork a thread and boot websocket and http combined server as a Wai app on Warp -}
runWebsocketAndHttp :: Network.WebSockets.ServerApp -> Network.Wai.Application -> Middlewares -> IO ()
runWebsocketAndHttp wsApp webApp middlewares = do
  let waiApp = websocketsOr defaultConnectionOptions wsApp webApp

  boot waiApp middlewares


boot :: Network.Wai.Application -> Middlewares -> IO ()
boot waiApp middlewares = do
  port <- Config.lookupEnv "PORT" 8081
  env  <- Config.lookupEnv "ENV" Config.Development

  let run = W.defaultSettings
              & W.setBeforeMainLoop (printStatus env port)
              & W.setPort port
              & W.runSettings

  _ <- forkIO $ run $ Config.logger env . middlewares $ waiApp

  pure ()


printStatus :: Config.Environment -> Int -> IO ()
printStatus env port = do
  putStrLn $ "Environment: " ++ show env
  putStrLn $ "Server started: http://localhost:" ++ show port


-- Middlewares
--
-- The following functions can be composed together and wrapped around a Wai app to
-- add specific middleware behavior. For example, a common full stack might look like;
--
--    middlewares = compression . staticFiles "public" . allowCsrf . corsified
--    runApp      = run port $ middlewares httpApp


defaultMiddlewares :: Network.Wai.Application -> Network.Wai.Application
defaultMiddlewares = compression . staticFiles "public" . allowCsrf . corsified


-- | Basic HTTP Auth
-- The following header will be set: @Access-Control-Allow-Headers: x-csrf-token@.
auth :: ByteString -> ByteString -> Middleware
auth username password = basicAuth (\u p -> pure $ u == username && p == password) "Authentication"


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
-- * RequestHeaders: @Content-Type, Authorization, Origin@
-- * MethodsAllowed: @OPTIONS, GET, PUT, POST@
appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy = CorsResourcePolicy
  { corsOrigins        = Nothing
  , corsMethods        = ["OPTIONS", "GET", "PUT", "POST", "DELETE"]
  , corsRequestHeaders = ["Authorization", "Content-Type", "Origin"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
  }
