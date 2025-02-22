module Test.Main where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Types (Inventory(..), InventoryResponse(..), MenuItem(..))
import Types.UUID (UUID)
import Yoga.JSON (readJSON_, writeJSON)

-- Read current inventory
handleReadInventory :: String -> Aff (Either String InventoryResponse)
handleReadInventory path = do
  content <- FS.readTextFile UTF8 path
  pure case readJSON_ content of
    Nothing -> Left "Failed to parse inventory"
    Just inventory -> Right $ InventoryData inventory

-- Create new menu item
handleWriteInventory :: String -> MenuItem -> Aff (Either String String)
handleWriteInventory path menuItem = do
  currentContent <- FS.readTextFile UTF8 path
  case readJSON_ currentContent of
    Nothing -> pure $ Left "Failed to parse current inventory"
    Just (Inventory items) -> do
      let
        newInventory = Inventory (items <> [ menuItem ])
        content = writeJSON newInventory
      FS.writeTextFile UTF8 path content
      pure $ Right "Successfully created item"

handleUpdateInventory :: String -> MenuItem -> Aff (Either String String)
handleUpdateInventory path updatedItem = do
  currentContent <- FS.readTextFile UTF8 path
  case readJSON_ currentContent of
    Nothing -> pure $ Left "Failed to parse current inventory"
    Just (Inventory items) -> do
      let
        newItems :: Array MenuItem
        newItems = map
          ( \(MenuItem item) ->
              let
                updatedSku :: UUID
                updatedSku = (unwrap updatedItem).sku

                currentSku :: UUID
                currentSku = item.sku
              in
                if currentSku == updatedSku then updatedItem
                else MenuItem item
          )
          items

        newInventory :: Inventory
        newInventory = Inventory newItems

        content :: String
        content = writeJSON newInventory

      FS.writeTextFile UTF8 path content
      pure $ Right "Successfully updated item"

-- Delete menu item
handleDeleteInventory :: String -> String -> Aff (Either String String)
handleDeleteInventory path itemId = do
  currentContent <- FS.readTextFile UTF8 path
  case readJSON_ currentContent of
    Nothing -> pure $ Left "Failed to parse current inventory"
    Just (Inventory items) -> do
      let
        newItems = filter (\(MenuItem item) -> show item.sku /= itemId) items
        newInventory = Inventory newItems
        content = writeJSON newInventory
      FS.writeTextFile UTF8 path content
      pure $ Right "Successfully deleted item"