module API where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Fetch (Method(..), fetch)
import Fetch.Yoga.Json (fromJSON)
import Types (Inventory, InventoryResponse(..), MenuItem)
import Yoga.JSON (writeJSON)

readInventory :: Aff (Either String InventoryResponse)
readInventory = do
  result <- attempt do
    response <- fetch "/api/inventory" 
      { method: GET
      , headers: { "Content-Type": "application/json" }
      }
    inventory <- fromJSON response.json :: Aff Inventory
    pure $ InventoryData inventory

  pure case result of
    Left err -> Left $ "Failed to read inventory: " <> show err
    Right inv -> Right inv

writeInventory :: MenuItem -> Aff (Either String InventoryResponse) 
writeInventory menuItem = do
  result <- attempt do
    let 
      content = writeJSON menuItem
      requestHeaders = { "Content-Type": "application/json" }
    
    liftEffect $ Console.log "Creating new menu item..."
    
    response <- fetch "/inventory"
      { method: POST
      , body: content
      , headers: requestHeaders
      }
    
    res <- fromJSON response.json :: Aff InventoryResponse
    pure res

  pure case result of
    Left err -> Left $ "Create error: " <> show err
    Right response -> Right response

updateInventory :: MenuItem -> Aff (Either String InventoryResponse)
updateInventory menuItem = do
  result <- attempt do
    let 
      content = writeJSON menuItem
      requestHeaders = { "Content-Type": "application/json" }
    
    liftEffect $ Console.log "Updating menu item..."
    
    response <- fetch "/api/inventory/update" 
      { method: PUT
      , body: content
      , headers: requestHeaders
      }
    
    res <- fromJSON response.json :: Aff InventoryResponse
    pure res

  pure case result of
    Left err -> Left $ "Update error: " <> show err
    Right response -> Right response

deleteInventory :: String -> Aff (Either String InventoryResponse)
deleteInventory itemId = do
  result <- attempt do
    response <- fetch ("/api/inventory/" <> itemId)
      { method: DELETE
      , headers: { "Content-Type": "application/json" }
      }
    res <- fromJSON response.json :: Aff InventoryResponse
    pure res

  pure case result of
    Left err -> Left $ "Delete error: " <> show err
    Right response -> Right response