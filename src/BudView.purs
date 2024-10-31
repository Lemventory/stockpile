module BudView where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.List.NonEmpty (NonEmptyList)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Fetch (Method(..), fetch)
import Fetch.Internal.RequestBody (class ToRequestBody)
import Fetch.Yoga.Json (fromJSON)
import Foreign (Foreign, ForeignError)
import Foreign.Index (readProp)
import JS.Fetch.RequestBody as RB
import Yoga.JSON (class ReadForeign, readImpl, unsafeStringify, writeImpl)

newtype ForeignRequestBody = ForeignRequestBody Foreign

data InventoryResponse
  = InventoryData Inventory
  | Message String

data QueryMode = JsonMode | HttpMode

newtype Inventory = Inventory (Array MenuItem)

newtype MenuItem = MenuItem
  { sort :: Int  
  , sku :: String
  , brand :: String
  , name :: String
  , price :: Number
  , quantity :: Int
  , category :: String
  , subcategory :: String
  , description :: String
  , tags :: Array String
  , metadata :: Metadata
  }

newtype Metadata = Metadata
  { thc :: String
  , cbd :: String
  , strain :: String
  , species :: String
  }

instance toRequestBodyForeignRequestBody :: ToRequestBody ForeignRequestBody where
  toRequestBody (ForeignRequestBody foreignValue) =
    RB.fromString (unsafeStringify foreignValue)

instance readForeignMenuItem :: ReadForeign MenuItem where
  readImpl json = do
    sort <- readProp "sort" json >>= readImpl
    sku <- readProp "sku" json >>= readImpl
    brand <- readProp "brand" json >>= readImpl
    name <- readProp "name" json >>= readImpl
    price <- readProp "price" json >>= readImpl
    quantity <- readProp "quantity" json >>= readImpl
    category <- readProp "category" json >>= readImpl
    subcategory <- readProp "subcategory" json >>= readImpl
    description <- readProp "description" json >>= readImpl
    tags <- readProp "tags" json >>= readImpl
    metadata <- readProp "metadata" json >>= readImpl
    pure $ MenuItem { sort, sku, brand, name, price, quantity, category, subcategory, description, tags, metadata }

instance readForeignMetadata :: ReadForeign Metadata where
  readImpl json = do
    thc <- readProp "thc" json >>= readImpl
    cbd <- readProp "cbd" json >>= readImpl
    strain <- readProp "strain" json >>= readImpl
    species <- readProp "species" json >>= readImpl

    pure $ Metadata { thc, cbd, strain, species }

instance readForeignInventory :: ReadForeign Inventory where
  readImpl json = do
    items <- readImpl json :: ExceptT (NonEmptyList ForeignError) Identity (Array MenuItem)
    pure (Inventory items)

fetchInventory :: QueryMode -> Aff (Either String InventoryResponse)
fetchInventory mode = case mode of
  JsonMode -> fetchInventoryFromJson
  HttpMode -> fetchInventoryFromHttp

-- Fetch Inventory from Local JSON
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

-- Fetch Inventory from HTTP API
fetchInventoryFromHttp :: Aff (Either String InventoryResponse)
fetchInventoryFromHttp = do
  result <- attempt do
    let requestHeaders = { "Content-Type": "application/json" }
    let requestBody = ForeignRequestBody (writeImpl { hello: "world" })
    
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