module MenuLiveView.DataFetcher where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Fetch (Method(..), fetch)
import Fetch.Yoga.Json (fromJSON)
import Types (Inventory, InventoryResponse(..))

-- | Data fetching mode
data QueryMode = JsonMode | HttpMode

derive instance eqQueryMode :: Eq QueryMode
derive instance ordQueryMode :: Ord QueryMode

instance Show QueryMode where
  show JsonMode = "JsonMode"
  show HttpMode = "HttpMode"

-- | Configuration type for fetcher
type FetchConfig =
  { apiEndpoint :: String -- Backend API endpoint
  , jsonPath :: String -- Path to JSON file
  , corsHeaders :: Boolean -- Whether to include CORS headers
  }

defaultConfig :: FetchConfig
defaultConfig =
  { apiEndpoint: "http://localhost:8080/inventory"
  , jsonPath: "./inventory.json"
  , corsHeaders: true
  }

-- | Fetch data from either JSON file or API endpoint
fetchInventory :: FetchConfig -> QueryMode -> Aff (Either String InventoryResponse)
fetchInventory config = case _ of
  JsonMode -> do
    liftEffect $ Console.log "Using JSON mode (local file)"
    fetchInventoryFromJson config
  HttpMode -> do
    liftEffect $ Console.log "Using HTTP mode (backend API)"
    fetchInventoryFromHttp config

-- | Fetch from local JSON file
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

-- | Fetch from Haskell backend API
fetchInventoryFromHttp :: FetchConfig -> Aff (Either String InventoryResponse)
fetchInventoryFromHttp config = do
  result <- attempt do
    liftEffect $ Console.log ("Fetching from API: " <> config.apiEndpoint)

    response <- fetch config.apiEndpoint
      { method: GET
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": "http://localhost:5174"
          }
      }

    inventoryResponse <- fromJSON response.json :: Aff InventoryResponse
    liftEffect $ Console.log "Got response from server"
    pure inventoryResponse

  case result of
    Left err -> do
      liftEffect $ Console.error $ "API fetch error details: " <> show err
      pure $ Left $ "API fetch error: " <> show err
    Right response -> pure $ Right response