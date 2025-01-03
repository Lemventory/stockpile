module API where

import Prelude

import Types (Inventory, InventoryResponse(..), MenuItem, QueryMode(..))
import Data.Either (Either(..))
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Fetch (Method(..), fetch)
import Fetch.Yoga.Json (fromJSON)
import Foreign (Foreign)
import Yoga.JSON (unsafeStringify, writeImpl)


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
      requestBody = unsafeStringify (writeImpl menuItem)
      requestHeaders = { "Content-Type": "application/json" }
      url = "/submit-menu-item.json" -- saves data to this dummy file for now

    liftEffect $ log ("Submitting item to JSON file at: " <> url)

    -- Send a POST request with JSON data
    coreResponse <- fetch url
      { method: POST
      , body: requestBody
      , headers: requestHeaders
      }
    
    -- Parse the response as JSON
    response <- fromJSON coreResponse.json :: Aff Foreign
    pure $ "Item saved successfully: " <> unsafeStringify response

  -- Return result as either a success message or an error
  case result of
    Left err -> pure $ Left $ "Error saving item: " <> show err
    Right msg -> pure $ Right $ Message msg


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