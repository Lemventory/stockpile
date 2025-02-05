{-# LANGUAGE OverloadedStrings #-}

module Server where

import API
import Control.Monad.IO.Class
import Data.UUID
import qualified Data.Pool as Pool
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
    :<|> deleteMenuItem
  where
    getInventory :: Handler InventoryResponse
    getInventory = do
      inventory <- liftIO $ getAllMenuItems pool
      return $ InventoryData inventory

    addMenuItem :: MenuItem -> Handler InventoryResponse
    addMenuItem item = do
      liftIO $ insertMenuItem pool item
      return $ Message "Item added successfully"

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