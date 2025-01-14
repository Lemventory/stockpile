module API where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Promise (toAff)
import Data.Array (length)
import Data.ArrayBuffer.Cast (toUint8Array)
import Data.ArrayBuffer.DataView as DV
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.TextDecoder (decodeUtf8)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Fetch (Method(..), fetch)
import Fetch.Yoga.Json (fromJSON)
import FileSystem (readLocalSystemFile, writeLocalSystemFile)
import Foreign (Foreign)
import Types (Inventory(..), InventoryResponse(..), MenuItem, QueryMode(..), StorageMethod(..))
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage as Storage
import Yoga.JSON (readJSON_, unsafeStringify, writeImpl, writeJSON)


-- | Main API functions that handle storage method selection
readInventory :: StorageMethod -> Aff (Either String InventoryResponse)
readInventory = case _ of
  LocalFile -> readLocalInventory
  BrowserStorage -> readBrowserInventory
  HttpApi -> readHttpInventory

writeInventory :: StorageMethod -> MenuItem -> Aff (Either String InventoryResponse)
writeInventory = case _ of
  LocalFile -> writeLocalInventory
  BrowserStorage -> writeBrowserInventory
  HttpApi -> writeHttpInventory

-- | Local File Storage Implementation
readLocalInventory :: Aff (Either String InventoryResponse)
readLocalInventory = do
  liftEffect $ Console.log "Starting readLocalInventory"
  bufferResult <- try $ toAff =<< liftEffect (readLocalSystemFile "inventorytest.json")
  case bufferResult of
    Left err -> pure $ Left $ "Failed to read file: " <> show err
    Right buffer -> do
      liftEffect $ Console.log "Got buffer from file"
      
      -- Create DataView from ArrayBuffer
      let dataView = DV.whole buffer
      
      -- Convert DataView to Uint8Array (returns Effect Uint8Array)
      uint8Array <- liftEffect $ toUint8Array dataView
      
      case decodeUtf8 uint8Array of
        Left err -> do
          liftEffect $ Console.error $ "UTF8 decode error: " <> show err
          pure $ Left $ "Failed to decode file: " <> show err
        Right content -> do
          liftEffect $ Console.log $ "File content: " <> content
          case readJSON_ content of
            Nothing -> do
              liftEffect $ Console.error "JSON parse error"
              pure $ Left "Invalid JSON in file"
            Just inventory -> do
              liftEffect $ Console.log "Successfully parsed inventory"
              pure $ Right $ InventoryData inventory

writeLocalInventory :: MenuItem -> Aff (Either String InventoryResponse)
writeLocalInventory menuItem = do
  liftEffect $ Console.log "Starting writeLocalInventory"
  currentInv <- readLocalInventory
  
  result <- try case currentInv of
    Left err -> do
      liftEffect $ Console.log "No existing inventory found, creating new one"
      let 
        newInventory = Inventory [menuItem]
        content = writeJSON newInventory
      void $ toAff =<< liftEffect (writeLocalSystemFile "inventorytest.json" content)
      pure $ Message "Created new inventory with item"
      
    Right (InventoryData (Inventory items)) -> do
      liftEffect $ Console.log $ "Current inventory has " <> show (length items) <> " items"
      let 
        newInventory = Inventory (items <> [menuItem])
        content = writeJSON newInventory
      void $ toAff =<< liftEffect (writeLocalSystemFile "inventorytest.json" content)
      pure $ Message "Successfully updated inventory"
      
    Right (Message msg) -> do
      liftEffect $ Console.log $ "Got message instead of inventory: " <> msg
      let 
        newInventory = Inventory [menuItem]
        content = writeJSON newInventory
      void $ toAff =<< liftEffect (writeLocalSystemFile "inventorytest.json" content)
      pure $ Message "Created new inventory with item"

  pure $ case result of
    Left err -> Left $ "Error writing inventory: " <> show err
    Right response -> Right response

-- | Browser Storage Implementation
readBrowserInventory :: Aff (Either String InventoryResponse)
readBrowserInventory = liftEffect do
  win <- window
  storage <- localStorage win
  mContent <- Storage.getItem "inventory" storage
  case mContent of 
    Nothing -> 
      pure $ Left "No inventory found in local storage"
    Just content -> 
      case readJSON_ content of
        Nothing -> 
          pure $ Left "Invalid JSON in local storage"
        Just inventory -> 
          pure $ Right $ InventoryData inventory

writeInventoryToJson :: MenuItem -> Aff (Either String InventoryResponse)
writeInventoryToJson menuItem = do
  result <- attempt do
    let 
      url = "/inventory.json"
      content = writeJSON menuItem
      requestHeaders = { "Content-Type": "application/json" }
    
    liftEffect $ Console.log ("Writing to URL: " <> url)
    liftEffect $ Console.log ("Content: " <> content)
    
    response <- fetch url 
      { method: POST
      , body: content
      , headers: requestHeaders
      }
    
    pure $ Message "Successfully wrote to file"

  case result of
    Left err -> pure $ Left $ "Write error: " <> show err
    Right msg -> pure $ Right msg

writeBrowserInventory :: MenuItem -> Aff (Either String InventoryResponse)
writeBrowserInventory menuItem = liftEffect do
  win <- window
  storage <- localStorage win
  
  mContent <- Storage.getItem "inventory" storage
  case mContent >>= readJSON_ of
    Nothing -> do
      let 
        newInventory = Inventory [menuItem]
        newContent = writeJSON newInventory
      Storage.setItem "inventory" newContent storage
      pure $ Right $ Message "Created new inventory with item"
    Just (Inventory items) -> do
      let 
        newInventory = Inventory (items <> [menuItem])
        newContent = writeJSON newInventory
      Storage.setItem "inventory" newContent storage
      pure $ Right $ Message "Successfully updated inventory"

fetchInventory :: QueryMode -> Aff (Either String InventoryResponse)
fetchInventory = case _ of
  JsonMode -> fetchInventoryFromJson
  HttpMode -> fetchInventoryFromHttp

-- | JSON file implementation
fetchInventoryFromJson :: Aff (Either String InventoryResponse)
fetchInventoryFromJson = do
  result <- attempt do
    timestamp <- liftEffect $ show <$> now
    let url = "/inventory.json?t=" <> timestamp
    liftEffect $ Console.log ("Fetching URL: " <> url)
    coreResponse <- fetch url {}
    inventory <- fromJSON coreResponse.json :: Aff Inventory
    pure inventory

  case result of
    Left err -> pure $ Left $ "Fetch error: " <> show err
    Right inventory -> pure $ Right $ InventoryData inventory

-- | HTTP API Implementation
readHttpInventory :: Aff (Either String InventoryResponse)
readHttpInventory = do
  result <- attempt do
    response <- fetch "https://api.example.com/inventory" 
      { method: GET
      , headers: { "Content-Type": "application/json" }
      }
    inventory <- fromJSON response.json :: Aff Inventory
    pure $ InventoryData inventory

  case result of
    Left err -> pure $ Left $ "HTTP fetch error: " <> show err
    Right inventory -> pure $ Right inventory

fetchInventoryFromHttp :: Aff (Either String InventoryResponse)
fetchInventoryFromHttp = do
  result <- attempt do
    let requestHeaders = { "Content-Type": "application/json" }
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

writeHttpInventory :: MenuItem -> Aff (Either String InventoryResponse)
writeHttpInventory menuItem = do
  let
    requestBody = writeJSON menuItem
    requestHeaders = { "Content-Type": "application/json" }

  result <- attempt do
    response <- fetch "https://api.example.com/inventory"
      { method: POST
      , body: requestBody
      , headers: requestHeaders
      }
    res <- fromJSON response.json :: Aff InventoryResponse
    pure res

  case result of
    Left err -> pure $ Left $ "HTTP post error: " <> show err
    Right res -> pure $ Right res


-- | Initialize empty inventory
initializeInventory :: StorageMethod -> Effect Unit
initializeInventory = case _ of
  LocalFile -> initializeLocalFile
  BrowserStorage -> initializeBrowser
  HttpApi -> pure unit  -- No initialization needed for HTTP API

initializeLocalFile :: Effect Unit
initializeLocalFile = do
  let emptyInventory = writeJSON (Inventory [])
  void $ writeLocalSystemFile "inventorytest.json" emptyInventory

initializeBrowser :: Effect Unit
initializeBrowser = do
  win <- window
  storage <- localStorage win
  mContent <- Storage.getItem "inventory" storage
  when (mContent == Nothing) do
    let emptyInventory = writeJSON (Inventory [])
    Storage.setItem "inventory" emptyInventory storage