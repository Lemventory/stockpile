{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server where

import API
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Pool as Pool
import Data.Text (pack)
import Database
import Database.PostgreSQL.Simple
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
    :<|> updateMenuItem
    :<|> deleteMenuItem pool 
  where
    getInventory :: Handler InventoryResponse
    getInventory = do
      inventory <- liftIO $ getAllMenuItems pool
      liftIO $ putStrLn "Sending inventory response:"
      liftIO $ LBS.putStrLn $ encode $ InventoryData inventory
      return $ InventoryData inventory

    addMenuItem :: MenuItem -> Handler InventoryResponse
    addMenuItem item = do
      liftIO $ putStrLn "Received request to add menu item"
      liftIO $ print item
      result <- liftIO $ try $ do
        insertMenuItem pool item
        let response = Message "Item added successfully"
        liftIO $ putStrLn $ "Sending response: " ++ show (encode response)
        return response
      case result of
        Right msg -> return msg
        Left (e :: SomeException) -> do
          let errMsg = pack $ "Error inserting item: " <> show e
          let response = Message errMsg
          liftIO $ putStrLn $ "Sending error response: " ++ show (encode response)
          return response

    updateMenuItem :: MenuItem -> Handler InventoryResponse
    updateMenuItem item = do
      liftIO $ putStrLn "Received request to update menu item"
      liftIO $ print item
      result <- liftIO $ try $ do
        _ <- updateExistingMenuItem pool item
        let response = Message "Item updated successfully"
        liftIO $ putStrLn $ "Sending response: " ++ show (encode response)
        return response
      case result of
        Right msg -> return msg
        Left (e :: SomeException) -> do
          let errMsg = pack $ "Error updating item: " <> show e
          let response = Message errMsg
          liftIO $ putStrLn $ "Sending error response: " ++ show (encode response)
          return response
