module App where

import API
import Database
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Servant
import System.Posix.User (getLoginName)
import Server
import qualified Data.ByteString.Char8 as BS

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

  putStrLn $ "Starting server on port " ++ show (serverPort config)
  
  let corsPolicy = CorsResourcePolicy
        { corsOrigins = Just ([BS.pack "http://localhost:5173", BS.pack "http://localhost:5174"], True)
        , corsMethods = [methodGet, methodPost, methodPut, methodDelete, methodOptions]
        , corsRequestHeaders = [hContentType, hAccept, hAuthorization, hContentLength]
        , corsExposedHeaders = Nothing
        , corsMaxAge = Just 3600
        , corsVaryOrigin = False
        , corsRequireOrigin = False
        , corsIgnoreFailures = False
        }

      app = cors (const $ Just corsPolicy) $ serve inventoryAPI (server pool)
  
  Warp.run (serverPort config) app