module API where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Fetch (Method(..), fetch)
import Fetch.Yoga.Json (fromJSON)
import Foreign (Foreign)
import Types (Inventory(..), InventoryResponse(..), MenuItem, QueryMode(..))
import Yoga.JSON (readJSON_, unsafeStringify, writeImpl, writeJSON)

fetchInventory :: QueryMode -> Aff (Either String InventoryResponse)
fetchInventory mode = case mode of
  JsonMode -> fetchInventoryFromJson
  HttpMode -> fetchInventoryFromHttp

fetchInventoryFromJson :: Aff (Either String InventoryResponse)
fetchInventoryFromJson = do
  result <- attempt do
    timestamp <- liftEffect $ show <$> now
    let url = "/inventory.json?t=" <> timestamp
    liftEffect $ log ("Fetching URL: " <> url)
    coreResponse <- fetch url {}
    inventory <- fromJSON coreResponse.json :: Aff Inventory
    pure inventory

  case result of
    Left err -> pure $ Left $ "Fetch error: " <> show err
    Right inventory -> pure $ Right $ InventoryData inventory

writeInventoryToJson :: MenuItem -> Aff (Either String InventoryResponse)
writeInventoryToJson menuItem = do
  result <- attempt do
    let 
      url = "/inventory.json"
      content = writeJSON menuItem
      requestHeaders = { "Content-Type": "application/json" }
    
    liftEffect $ log ("Writing to URL: " <> url)
    liftEffect $ log ("Content: " <> content)
    
    response <- fetch url 
      { method: POST
      , body: content
      , headers: requestHeaders
      }
    
    pure $ Message "Successfully wrote to file"

  pure case result of
    Left err -> Left $ "Write error: " <> show err
    Right msg -> Right msg

updateInventoryInJson :: MenuItem -> Aff (Either String InventoryResponse)
updateInventoryInJson menuItem = do
  currentInventory <- fetchInventoryFromJson
  
  case currentInventory of
    Left err -> pure $ Left $ "Error reading current inventory: " <> err
    Right (InventoryData (Inventory items)) -> do
      let
        updatedItems = items <> [menuItem]
        updatedInventory = Inventory updatedItems
        content = writeJSON updatedInventory
        url = "/inventory.json"
        requestHeaders = { "Content-Type": "application/json" }
      
      result <- attempt do
        liftEffect $ log ("Updating URL: " <> url)
        response <- fetch url 
          { method: POST
          , body: content
          , headers: requestHeaders
          }
        pure $ Message "Successfully updated inventory"
      
      pure case result of
        Left err -> Left $ "Update error: " <> show err
        Right msg -> Right msg
    Right (Message _) -> pure $ Left "Unexpected response format"

fetchInventoryFromHttp :: Aff (Either String InventoryResponse)
fetchInventoryFromHttp = do
  result <- attempt do
    let requestHeaders = { "Content-Type": "application/json" }
    -- Convert ForeignRequestBody to a JSON string
    let requestBody = unsafeStringify (writeImpl { hello: "world" })
    coreResponse <- fetch "https://httpbin.org/post"
      { method: POST
      , body: requestBody
      , headers: requestHeaders
      }
    res <- fromJSON coreResponse.json :: Aff Foreign
    pure $ "Received response: " <> unsafeStringify res

  case result of
    Left err -> pure $ Left $ "Fetch error: " <> show err
    Right msg -> pure $ Right $ Message msg

postInventoryToJson :: MenuItem -> Aff (Either String InventoryResponse)
postInventoryToJson menuItem = do
  result <- attempt do
    let 
      requestBody = writeJSON menuItem
      requestHeaders = { "Content-Type": "application/json" }
      url = "/inventorytest.json"
    liftEffect do
      log ("Submitting item to JSON file at: " <> url)
      log ("Request body: " <> requestBody)

    response <- fetch url
      { method: POST
      , body: requestBody
      , headers: requestHeaders
      }

    -- Handle different response scenarios
    if response.status == 204 
      then pure $ Right $ Message "Item saved successfully (no content)"
      else do
        -- Try to parse response as InventoryResponse
        let responseText = unsafeStringify response.json
        case readJSON_ responseText :: Maybe InventoryResponse of
          Just resp -> pure $ Right resp
          Nothing -> case response.status of
            200 -> pure $ Right $ Message "Item saved successfully"
            status -> pure $ Left $ "Unexpected response status: " <> show status

  pure case result of
    Left err -> Left $ "Error saving item: " <> show err
    Right (Left err) -> Left err
    Right (Right msg) -> Right msg

postInventoryToHttp :: MenuItem -> Aff (Either String InventoryResponse)
postInventoryToHttp menuItem = do
  let
    -- Convert `MenuItem` to JSON
    requestBody = unsafeStringify (writeImpl menuItem)
    requestHeaders = { "Content-Type": "application/json" }

  result <- attempt do
    -- POST request with JSON body
    coreResponse <- fetch "https://your-api-endpoint.com/submit-menu-item"
      { method: POST
      , body: requestBody
      , headers: requestHeaders
      }
    response <- fromJSON coreResponse.json :: Aff Foreign
    pure $ "Item submitted successfully: " <> unsafeStringify response

  case result of
    Left err -> pure $ Left $ "Submission error: " <> show err
    Right msg -> pure $ Right $ Message msg