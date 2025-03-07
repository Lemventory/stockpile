module API.Inventory where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Fetch (Method(..), fetch)
import Fetch.Yoga.Json (fromJSON)
import NetworkConfig (currentConfig)
import Types.Inventory (Inventory, InventoryResponse(..), MenuItem)
import Types.LiveViewConfig (QueryMode(..), FetchConfig)
import Yoga.JSON (writeJSON)

baseUrl :: String
baseUrl = currentConfig.apiBaseUrl

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
          , "Origin": currentConfig.appOrigin
          }
      }
    fromJSON response.json

  pure case result of
    Left err -> Left $ "Create error: " <> show err
    Right response -> Right response

readInventory :: Aff (Either String InventoryResponse)
readInventory = do
  result <- attempt do
    liftEffect $ Console.log $ "Fetching inventory from: " <> baseUrl <>
      "/inventory"
    response <- fetch (baseUrl <> "/inventory")
      { method: GET
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": currentConfig.appOrigin
          }
      }
    liftEffect $ Console.log "Got response, parsing JSON..."
    inventoryResponse <- fromJSON response.json :: Aff InventoryResponse
    liftEffect $ Console.log "Successfully parsed inventory response"
    pure inventoryResponse

  pure case result of
    Left err -> Left $ "Failed to read inventory: " <> show err
    Right response -> Right response

updateInventory :: MenuItem -> Aff (Either String InventoryResponse)
updateInventory menuItem = do
  result <- attempt do
    let content = writeJSON menuItem
    liftEffect $ Console.log "Updating menu item..."
    liftEffect $ Console.log $ "Sending content: " <> content

    response <- fetch (baseUrl <> "/inventory")
      { method: PUT
      , body: content
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": currentConfig.appOrigin
          }
      }

    fromJSON response.json :: Aff InventoryResponse

  pure case result of
    Left err -> Left $ "Update error: " <> show err
    Right response -> Right response

deleteInventory :: String -> Aff (Either String InventoryResponse)
deleteInventory itemId = do
  result <- attempt do
    response <- fetch (baseUrl <> "/inventory/" <> itemId)
      { method: DELETE
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": currentConfig.appOrigin
          }
      }
    fromJSON response.json :: Aff InventoryResponse

  pure case result of
    Left err -> Left $ "Delete error: " <> show err
    Right response -> Right response

fetchInventoryFromJson :: FetchConfig -> Aff (Either String InventoryResponse)
fetchInventoryFromJson config = do
  result <- attempt do
    timestamp <- liftEffect $ show <$> now
    let url = config.jsonPath <> "?t=" <> timestamp
    liftEffect $ Console.log ("Fetching from JSON: " <> url)

    response <- fetch url {}
    inventory <- fromJSON response.json :: Aff Inventory
    pure inventory

  pure case result of
    Left err -> Left $ "JSON fetch error: " <> show err
    Right inventory -> Right $ InventoryData inventory

fetchInventoryFromHttp :: FetchConfig -> Aff (Either String InventoryResponse)
fetchInventoryFromHttp config = do
  liftEffect $ Console.log "Starting HTTP fetch..."
  liftEffect $ Console.log $ "Using endpoint: " <> config.apiEndpoint
  result <- attempt do
    liftEffect $ Console.log "Making fetch request..."

    response <- fetch config.apiEndpoint
      { method: GET
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": currentConfig.appOrigin
          }
      }

    liftEffect $ Console.log "Got response, parsing JSON..."
    parsed <- fromJSON response.json :: Aff InventoryResponse
    liftEffect $ Console.log "Successfully parsed response"
    pure parsed

  case result of
    Left err -> do
      liftEffect $ Console.error $ "API fetch error details: " <> show err
      pure $ Left $ "API fetch error: " <> show err
    Right response -> do
      liftEffect $ Console.log "Success: Got inventory data"
      pure $ Right response

fetchInventory
  :: FetchConfig -> QueryMode -> Aff (Either String InventoryResponse)
fetchInventory config = case _ of
  JsonMode -> do
    liftEffect $ Console.log "Using JSON mode (local file)"
    fetchInventoryFromJson config
  HttpMode -> do
    liftEffect $ Console.log "Using HTTP mode (backend API)"
    fetchInventoryFromHttp config


-- recordTransaction :: Transaction -> Aff (Either String String)
-- recordTransaction transaction = do
--   let tx = unwrap transaction
--   liftEffect $ log $ "Recording transaction: " <> uuidToString tx.id
  
--   -- In a real implementation, this would send the transaction to the backend
--   -- For now, we'll simulate a successful response
--   pure $ Right $ "Transaction recorded successfully: " <> uuidToString tx.id