module API where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Fetch (Method(..), RequestMode(..), RequestCredentials(..), fetch)
import Fetch.Yoga.Json (fromJSON)
import Types (Inventory, InventoryResponse(..), MenuItem)
import Yoga.JSON (writeJSON)

baseUrl :: String
baseUrl = "http://localhost:8080"  -- Match Haskell server port

writeInventory :: MenuItem -> Aff (Either String InventoryResponse) 
writeInventory menuItem = do
  result <- attempt do
    let content = writeJSON menuItem
    
    liftEffect $ Console.log "Creating new menu item..."
    liftEffect $ Console.log $ "Sending content: " <> content
    
    response <- fetch (baseUrl <> "/inventory")
      { method: POST
      , body: content
      , headers: 
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          }
      , mode: Cors
      , credentials: Omit
      }
    
    -- Get the raw response and log it
    responseText <- response.text
    liftEffect $ Console.log "Raw server response:"
    liftEffect $ Console.log responseText

    -- Parse the response JSON directly
    res <- fromJSON response.json :: Aff InventoryResponse
    pure res

  pure case result of
    Left err -> Left $ "Create error: " <> show err
    Right response -> Right response

readInventory :: Aff (Either String InventoryResponse)
readInventory = do
  result <- attempt do
    response <- fetch (baseUrl <> "/inventory")
      { method: GET
      , headers: { "Content-Type": "application/json" }
      , mode: Cors
      , credentials: Omit
      }
    inventory <- fromJSON response.json :: Aff Inventory
    pure $ InventoryData inventory

  pure case result of
    Left err -> Left $ "Failed to read inventory: " <> show err
    Right inv -> Right inv

updateInventory :: MenuItem -> Aff (Either String InventoryResponse)
updateInventory menuItem = do
  result <- attempt do
    let content = writeJSON menuItem
    
    liftEffect $ Console.log "Updating menu item..."
    
    response <- fetch (baseUrl <> "/inventory") 
      { method: PUT
      , body: content
      , headers: { "Content-Type": "application/json" }
      , mode: Cors
      , credentials: Omit
      }
    
    res <- fromJSON response.json :: Aff InventoryResponse
    pure res

  pure case result of
    Left err -> Left $ "Update error: " <> show err 
    Right response -> Right response

deleteInventory :: String -> Aff (Either String InventoryResponse)
deleteInventory itemId = do
  result <- attempt do
    response <- fetch (baseUrl <> "/inventory/" <> itemId)
      { method: DELETE
      , headers: { "Content-Type": "application/json" }
      , mode: Cors
      , credentials: Omit
      }
    res <- fromJSON response.json :: Aff InventoryResponse
    pure res

  pure case result of
    Left err -> Left $ "Delete error: " <> show err
    Right response -> Right response