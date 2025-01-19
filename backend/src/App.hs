module App where

import Network.Wai.Handler.Warp
import Servant
import Database
import Server
import API

run :: IO ()
run = do
  let config = AppConfig
        { dbConfig = DBConfig
            { dbHost = "localhost"
            , dbPort = 5432
            , dbName = "cannabis_inventory"
            , dbUser = "postgres"
            , dbPassword = "postgres"
            }
        , serverPort = 8080
        }
  
  conn <- initializeDB (dbConfig config)
  createTables conn
  
  putStrLn $ "Starting server on port " ++ show (serverPort config)
  run (serverPort config) $ serve inventoryAPI (server conn)