-- FILE: backend/src/App.hs
module App where

import API
import Database
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Servant
import Server
import System.Posix.User (getLoginName)

run :: IO ()
run = do
  currentUser <- getLoginName
  let config =
        AppConfig
          { dbConfig =
              DBConfig
                { dbHost = "localhost"
                , dbPort = 5432
                , dbName = "cheeblr"
                , dbUser = currentUser
                , dbPassword = "postgres"
                , poolSize = 10
                }
          , serverPort = 8080
          }

  pool <- initializeDB (dbConfig config)
  createTables pool

  putStrLn $ "Starting server on all interfaces, port " ++ show (serverPort config)
  putStrLn "=================================="
  putStrLn $ "Server running on port " ++ show (serverPort config)
  putStrLn "You can access this application from other devices on your network using:"
  putStrLn $ "http://YOUR_MACHINE_IP:" ++ show (serverPort config)
  putStrLn "=================================="

  let
    corsPolicy =
      CorsResourcePolicy
        { corsOrigins = Nothing  -- Allow requests from any origin for LAN testing
        , corsMethods = [methodGet, methodPost, methodPut, methodDelete, methodOptions]
        , corsRequestHeaders = [hContentType, hAccept, hAuthorization, hOrigin, hContentLength]
        , corsExposedHeaders = Nothing
        , corsMaxAge = Just 3600
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }

    app = cors (const $ Just corsPolicy) $ serve inventoryAPI (server pool)

  -- Use the defined app variable
  Warp.run (serverPort config) app