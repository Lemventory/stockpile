{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Servant
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Types
import API
import Database
import Data.UUID

data AppConfig = AppConfig
  { dbConfig :: DBConfig
  , serverPort :: Int
  }

server :: Connection -> Server InventoryAPI
server conn = getInventory 
        :<|> addMenuItem
        :<|> deleteMenuItem
  where
    getInventory :: Handler InventoryResponse
    getInventory = do
      inventory <- liftIO $ getAllMenuItems conn
      return $ InventoryData inventory

    addMenuItem :: MenuItem -> Handler InventoryResponse
    addMenuItem item = do
      liftIO $ insertMenuItem conn item
      return $ Message "Item added successfully"

    deleteMenuItem :: UUID -> Handler InventoryResponse
    deleteMenuItem sku = do
      affected <- liftIO $ execute conn 
        "DELETE FROM menu_items WHERE sku = ?" 
        (Only sku)
      if affected > 0
        then return $ Message "Item deleted successfully"
        else throwError err404