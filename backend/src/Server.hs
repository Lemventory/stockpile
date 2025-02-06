{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import API
import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as LBS -- Add this import
import qualified Data.Pool as Pool
import Data.UUID
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
      liftIO $ putStrLn "Received request to add menu item"
      liftIO $ print item
      result <-
        liftIO $
          catch
            ( do
                insertMenuItem pool item
                return $ Right "Item added successfully"
            )
            ( \(e :: SomeException) -> do
                putStrLn $ "Error inserting item: " ++ show e
                return $ Left (show e)
            )
      case result of
        Right msg -> return $ Message msg
        Left errMsg -> throwError err500 {errBody = LBS.pack $ "Error inserting item: " ++ errMsg}

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
