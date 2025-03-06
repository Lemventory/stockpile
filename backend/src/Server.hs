{-# LANGUAGE ScopedTypeVariables #-}
module Server where

import API.Inventory
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.UUID (UUID)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Database.PostgreSQL.Simple
import Data.Pool (Pool)
import Servant
import DB.Database (getAllMenuItems, insertMenuItem, updateExistingMenuItem, deleteMenuItem)
import Types.Inventory
import API.Transaction (PosAPI)

-- Temporary implementation until we create Server.Transaction module
posServer :: Pool Connection -> Server PosAPI
posServer _ = error "Transaction API not implemented yet"

server :: Pool Connection -> Server InventoryAPI
server pool =
  getInventory
    :<|> addMenuItem
    :<|> updateMenuItem
    :<|> deleteMenuItem
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
        Left e -> do
          let errMsg = "Error inserting item: " <> show e
          let response = Message errMsg
          liftIO $ putStrLn $ "Sending error response: " ++ show (encode response)
          return response

    updateMenuItem :: MenuItem -> Handler InventoryResponse
    updateMenuItem item = do
      liftIO $ putStrLn "Received request to update menu item"
      liftIO $ print item
      result <- liftIO $ try $ do
        updateExistingMenuItem pool item
        let response = Message "Item updated successfully"
        liftIO $ putStrLn $ "Sending response: " ++ show (encode response)
        return response
      case result of
        Right msg -> return msg
        Left e -> do
          let errMsg = "Error updating item: " <> show e
          let response = Message errMsg
          liftIO $ putStrLn $ "Sending error response: " ++ show (encode response)
          return response

    deleteMenuItem :: UUID -> Handler InventoryResponse
    deleteMenuItem = deleteMenuItem pool

combinedServer :: Pool Connection -> Server API
combinedServer pool =
  server pool
    :<|> posServer pool