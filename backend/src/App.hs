module App where

import API
import Database
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Servant
import Server

run :: IO ()
run = do
  let config =
        AppConfig
          { dbConfig =
              DBConfig
                { dbHost = "localhost"
                , dbPort = 5432
                , dbName = "cheeblr"
                , dbUser = "postgres"
                , dbPassword = "postgres"
                , poolSize = 10
                }
          , serverPort = 8080
          }

  pool <- initializeDB (dbConfig config)
  createTables pool

  putStrLn $ "Starting server on port " ++ show (serverPort config)
  Warp.run (serverPort config) $
    simpleCors $
      serve inventoryAPI (server pool)
