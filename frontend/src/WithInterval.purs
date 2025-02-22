module MenuLiveView
  ( runLiveView)
  where

import Prelude

import Types (Inventory(..), InventoryResponse(..), ItemCategory, MenuItem(..), QueryMode(..), StrainLineage(..), Species)
import API (fetchInventory)

import Data.Array (filter, sortBy)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), replace, toLower)
import Data.String.Pattern (Replacement(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut, text_)
import Deku.DOM (img)
import Deku.DOM as D
import Deku.DOM.Attributes (alt_, klass_, src_)
import Deku.Effect (useState)
import Deku.Hooks ((<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FRP.Event (subscribe)
import FRP.Event.Time (interval)

-- Sorting Configuration
data SortField =  SortByOrder
                | SortByName 
                | SortByCategory 
                | SortBySubCategory 
                | SortBySpecies
                | SortBySKU
                | SortByPrice 
                | SortByQuantity

data SortOrder = Ascending | Descending

type Config =
  { sortFields :: Array (Tuple SortField SortOrder) 
  , hideOutOfStock :: Boolean
  , mode :: QueryMode
  , refreshRate :: Int
  , screens :: Int
  }

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT

compareMenuItems :: Config -> MenuItem -> MenuItem -> Ordering
compareMenuItems config (MenuItem item1) (MenuItem item2) =
  let
    StrainLineage meta1 = item1.strain_lineage
    StrainLineage meta2 = item2.strain_lineage

    compareByField :: Tuple SortField SortOrder -> Ordering
    compareByField (sortField /\ sortOrder) =
      let
        fieldComparison = case sortField of
          SortByOrder -> compare item1.sort item2.sort
          SortByName -> compare item1.name item2.name
          SortByCategory -> compare item1.category item2.category
          SortBySubCategory -> compare item1.subcategory item2.subcategory
          SortBySpecies -> compare meta1.species meta2.species
          SortBySKU -> compare item1.sku item2.sku
          SortByPrice -> compare item1.price item2.price
          SortByQuantity -> compare item1.quantity item2.quantity
      in
        case sortOrder of
          Ascending -> fieldComparison
          Descending -> invertOrdering fieldComparison

    compareWithPriority :: Array (Tuple SortField SortOrder) -> Ordering
    compareWithPriority priorities = case Array.uncons priorities of
      Nothing -> EQ
      Just { head : priority, tail : rest } ->
        case compareByField priority of
          EQ -> compareWithPriority rest
          result -> result
  in
    compareWithPriority config.sortFields

renderInventory :: Config -> Inventory -> Nut
renderInventory config (Inventory items) = D.div
  [ klass_ "inventory-grid" ]
  (map renderItem sortedItems)
  where
    filteredItems = if config.hideOutOfStock
      then filter (\(MenuItem item) -> item.quantity > 0) items
      else items
    sortedItems = sortBy (compareMenuItems config) filteredItems

renderItem :: MenuItem -> Nut
renderItem (MenuItem item) = 
  let
    StrainLineage meta = item.strain_lineage
  in
  D.div
    [ klass_ ("inventory-item-card " <> generateClassName 
        { category: item.category
        , subcategory: item.subcategory
        , species: meta.species
        }) 
    ]
    [ D.div [ klass_ "item-header" ]
        [ D.div []
            [ D.div [ klass_ "item-brand" ] [ text_ item.brand ]
            , D.div [ klass_ "item-name" ] [ text_ ("'" <> item.name <> "'") ]
            ]
        , D.div [ klass_ "item-img" ] [ img [ alt_ "weed pic", src_ meta.img ] [] ]
        ]
    , D.div [ klass_ "item-category" ] [ text_ (show item.category <> " - " <> item.subcategory) ]
    , D.div [ klass_ "item-species" ] [ text_ ("Species: " <> show meta.species) ]
    , D.div [ klass_ "item-strain_lineage" ] [ text_ ("Strain: " <> meta.strain) ]
    , D.div [ klass_ "item-price" ] [ text_ ("$" <> show item.price <> " (" <> item.per_package <> "" <> item.measure_unit <> ")") ]
    , D.div [ klass_ "item-quantity" ] [ text_ ("in stock: " <> show item.quantity) ]
    ]

generateClassName :: { category :: ItemCategory, subcategory :: String, species :: Species } -> String
generateClassName item =
  "species-" <> toClassName (show item.species) <> 
  " category-" <> toClassName (show item.category) <> 
  " subcategory-" <> toClassName item.subcategory

toClassName :: String -> String
toClassName str = toLower (replace (Pattern " ") (Replacement "-") str)

liveView :: Effect Unit
liveView = do
  setInventory /\ inventory <- useState (Inventory [])
  let
    config =
      { sortFields: [ SortByCategory /\ Ascending
                    , SortBySpecies /\ Descending
                    , SortByQuantity /\ Descending
                    ]
      , hideOutOfStock: true
      , mode: JsonMode
      , refreshRate: 3000
      , screens: 1
      }

    fetchAndUpdateInventory :: Effect Unit
    fetchAndUpdateInventory = launchAff_ do
      result <- fetchInventory config.mode
      liftEffect $ case result of
        Left err -> log ("Error fetching inventory: " <> err)
        Right (InventoryData inv) -> setInventory inv
        Right (Message msg) -> log ("Message: " <> msg)

  _ <- fetchAndUpdateInventory

  do
    { event: tickEvent} <- interval config.refreshRate
    void $ subscribe tickEvent \_ -> do
      fetchAndUpdateInventory

  -- Run Deku UI
  void $ runInBody $ Deku.do
    D.div [] [ inventory <#~> renderInventory config ]

runLiveView :: Effect Unit
runLiveView = do
  log "Starting main"
  liveView


-- Fetching as it was:
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
module MenuLiveView
  ( runLiveView)
  where

import Prelude

import Types (Inventory(..), InventoryResponse(..), ItemCategory, MenuItem(..), QueryMode(..), StrainLineage(..), Species)
import API (fetchInventory)

import Data.Array (filter, sortBy)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), replace, toLower)
import Data.String.Pattern (Replacement(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut, text_)
import Deku.DOM (img)
import Deku.DOM as D
import Deku.DOM.Attributes (alt_, klass_, src_)
import Deku.Effect (useState)
import Deku.Hooks ((<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FRP.Event (subscribe)
import FRP.Event.Time (interval)

-- Sorting Configuration
data SortField =  SortByOrder
                | SortByName 
                | SortByCategory 
                | SortBySubCategory 
                | SortBySpecies
                | SortBySKU
                | SortByPrice 
                | SortByQuantity

data SortOrder = Ascending | Descending

type Config =
  { sortFields :: Array (Tuple SortField SortOrder) 
  , hideOutOfStock :: Boolean
  , mode :: QueryMode
  , refreshRate :: Int
  , screens :: Int
  }

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT

compareMenuItems :: Config -> MenuItem -> MenuItem -> Ordering
compareMenuItems config (MenuItem item1) (MenuItem item2) =
  let
    StrainLineage meta1 = item1.strain_lineage
    StrainLineage meta2 = item2.strain_lineage

    compareByField :: Tuple SortField SortOrder -> Ordering
    compareByField (sortField /\ sortOrder) =
      let
        fieldComparison = case sortField of
          SortByOrder -> compare item1.sort item2.sort
          SortByName -> compare item1.name item2.name
          SortByCategory -> compare item1.category item2.category
          SortBySubCategory -> compare item1.subcategory item2.subcategory
          SortBySpecies -> compare meta1.species meta2.species
          SortBySKU -> compare item1.sku item2.sku
          SortByPrice -> compare item1.price item2.price
          SortByQuantity -> compare item1.quantity item2.quantity
      in
        case sortOrder of
          Ascending -> fieldComparison
          Descending -> invertOrdering fieldComparison

    compareWithPriority :: Array (Tuple SortField SortOrder) -> Ordering
    compareWithPriority priorities = case Array.uncons priorities of
      Nothing -> EQ
      Just { head : priority, tail : rest } ->
        case compareByField priority of
          EQ -> compareWithPriority rest
          result -> result
  in
    compareWithPriority config.sortFields

renderInventory :: Config -> Inventory -> Nut
renderInventory config (Inventory items) = D.div
  [ klass_ "inventory-grid" ]
  (map renderItem sortedItems)
  where
    filteredItems = if config.hideOutOfStock
      then filter (\(MenuItem item) -> item.quantity > 0) items
      else items
    sortedItems = sortBy (compareMenuItems config) filteredItems

renderItem :: MenuItem -> Nut
renderItem (MenuItem item) = 
  let
    StrainLineage meta = item.strain_lineage
  in
  D.div
    [ klass_ ("inventory-item-card " <> generateClassName 
        { category: item.category
        , subcategory: item.subcategory
        , species: meta.species
        }) 
    ]
    [ D.div [ klass_ "item-header" ]
        [ D.div []
            [ D.div [ klass_ "item-brand" ] [ text_ item.brand ]
            , D.div [ klass_ "item-name" ] [ text_ ("'" <> item.name <> "'") ]
            ]
        , D.div [ klass_ "item-img" ] [ img [ alt_ "weed pic", src_ meta.img ] [] ]
        ]
    , D.div [ klass_ "item-category" ] [ text_ (show item.category <> " - " <> item.subcategory) ]
    , D.div [ klass_ "item-species" ] [ text_ ("Species: " <> show meta.species) ]
    , D.div [ klass_ "item-strain_lineage" ] [ text_ ("Strain: " <> meta.strain) ]
    , D.div [ klass_ "item-price" ] [ text_ ("$" <> show item.price <> " (" <> item.per_package <> "" <> item.measure_unit <> ")") ]
    , D.div [ klass_ "item-quantity" ] [ text_ ("in stock: " <> show item.quantity) ]
    ]

generateClassName :: { category :: ItemCategory, subcategory :: String, species :: Species } -> String
generateClassName item =
  "species-" <> toClassName (show item.species) <> 
  " category-" <> toClassName (show item.category) <> 
  " subcategory-" <> toClassName item.subcategory

toClassName :: String -> String
toClassName str = toLower (replace (Pattern " ") (Replacement "-") str)

liveView :: Effect Unit
liveView = do
  setInventory /\ inventory <- useState (Inventory [])
  let
    config =
      { sortFields: [ SortByCategory /\ Ascending
                    , SortBySpecies /\ Descending
                    , SortByQuantity /\ Descending
                    ]
      , hideOutOfStock: true
      , mode: JsonMode
      , refreshRate: 3000
      , screens: 1
      }

    fetchAndUpdateInventory :: Effect Unit
    fetchAndUpdateInventory = launchAff_ do
      result <- fetchInventory config.mode
      liftEffect $ case result of
        Left err -> log ("Error fetching inventory: " <> err)
        Right (InventoryData inv) -> setInventory inv
        Right (Message msg) -> log ("Message: " <> msg)

  _ <- fetchAndUpdateInventory

  do
    { event: tickEvent} <- interval config.refreshRate
    void $ subscribe tickEvent \_ -> do
      fetchAndUpdateInventory

  -- Run Deku UI
  void $ runInBody $ Deku.do
    D.div [] [ inventory <#~> renderInventory config ]

runLiveView :: Effect Unit
runLiveView = do
  log "Starting main"
  liveView