{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import API
import Control.Exception (SomeException, catch, try)
import Control.Monad.IO.Class
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Pool as Pool
import Data.Text (Text, pack)
import Data.UUID
import Database
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query (..))
import Servant
import Types

data AppConfig = AppConfig
  { dbConfig :: DBConfig
  , serverPort :: Int
  }

server :: Pool.Pool Connection -> Server InventoryAPI
server pool =
  getInventory
    :<|> addMenuItem
    :<|> deleteMenuItem
  where
    getInventory :: Handler InventoryResponse
    getInventory = do
      inventory <- liftIO $ getAllMenuItems pool
      return $ InventoryData inventory

    addMenuItem :: MenuItem -> Handler InventoryResponse
    addMenuItem item = do
      liftIO $ putStrLn "Received request to add menu item"
      liftIO $ print item
      result <- liftIO $ try $ do
        insertMenuItem pool item
        let response = Message "Item added successfully"
        liftIO $ putStrLn $ "Sending response: " ++ show (encode response) -- Add this line
        return response
      case result of
        Right msg -> return msg
        Left (e :: SomeException) -> do
          let errMsg = pack $ "Error inserting item: " <> show e
          let response = Message errMsg
          liftIO $ putStrLn $ "Sending error response: " ++ show (encode response) -- Add this line
          return response

    deleteMenuItem :: UUID -> Handler InventoryResponse
    deleteMenuItem uuid = do
      affected <-
        liftIO $ withConnection pool $ \conn ->
          execute
            conn
            "DELETE FROM menu_items WHERE sku = ?"
            (Only uuid)
      if affected > 0
        then return $ Message "Item deleted successfully"
        else throwError err404
