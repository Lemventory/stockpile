module WithInterval where

import Prelude

import Data.Array (filter, length, sortBy)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), replace, toLower)
import Data.String.Pattern (Replacement(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.Effect (useState)
import Deku.Hooks ((<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_, Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Types (Inventory(..), InventoryResponse(..), ItemCategory, MenuItem(..), StrainLineage(..), Species)
import Effect.Now (now)
import Fetch (Method(..), fetch)
import Fetch.Yoga.Json (fromJSON)


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


-- | Sorting types
data SortField
  = SortByOrder
  | SortByName
  | SortByCategory
  | SortBySubCategory
  | SortBySpecies
  | SortBySKU
  | SortByPrice
  | SortByQuantity

data SortOrder = Ascending | Descending

-- | Unified configuration
type Config =
  { sortFields :: Array (Tuple SortField SortOrder)
  , hideOutOfStock :: Boolean
  , mode :: QueryMode
  , refreshRate :: Int
  , screens :: Int
  , fetchConfig :: FetchConfig
  }

defaultViewConfig :: Config
defaultViewConfig =
  { sortFields:
      [ SortByCategory /\ Ascending
      , SortBySpecies /\ Descending
      , SortByQuantity /\ Descending
      ]
  , hideOutOfStock: false
  , mode: HttpMode
  , refreshRate: 5000
  , screens: 1
  , fetchConfig: defaultConfig
      { apiEndpoint = "http://localhost:8080/inventory"
      , corsHeaders = true
      }
  }

-- | Main view logic
liveView :: Effect Unit
liveView = do
  setInventory /\ inventory <- useState (Inventory [])
  setLoading /\ loading <- useState true
  setError /\ error <- useState ""

  let
    config = defaultViewConfig

    fetchAndUpdateInventory :: Effect Unit
    fetchAndUpdateInventory = launchAff_ do
      liftEffect $ setLoading true
      liftEffect $ setError ""
      liftEffect $ Console.log $ "Fetching inventory with mode: " <> show config.mode

      result <- fetchInventory config.fetchConfig config.mode

      liftEffect $ case result of
        Left err -> do
          Console.error $ "Error fetching inventory: " <> err
          setError err
          setLoading false

        Right (InventoryData inv@(Inventory items)) -> do
          Console.log $ "Received " <> show (length items) <> " items"
          setInventory inv
          setLoading false

        Right (Message msg) -> do
          Console.log msg
          setError msg
          setLoading false

  -- Initial fetch
  void fetchAndUpdateInventory

  -- Run Deku UI
  void $ runInBody Deku.do
    D.div []
      [ D.div
          [ DA.klass_ "status-container" ]
          [ loading <#~> \isLoading ->
              if isLoading then text_ "Loading..."
              else text_ ""
          , error <#~> \err ->
              if err /= "" then text_ ("Error: " <> err)
              else text_ ""
          ]
      , D.div
          [ DA.klass_ "inventory-container" ]
          [ inventory <#~> renderInventory config ]
      ]

-- | Rendering functions
renderInventory :: Config -> Inventory -> Nut
renderInventory config (Inventory items) =
  let
    filteredItems =
      if config.hideOutOfStock then filter (\(MenuItem item) -> item.quantity > 0) items
      else items

    sortedItems = sortBy (compareMenuItems config) filteredItems
  in
    D.div
      [ DA.klass_ "inventory-grid" ]
      (map renderItem sortedItems)

renderItem :: MenuItem -> Nut
renderItem (MenuItem record) =
  let
    StrainLineage meta = record.strain_lineage
    className = generateClassName
      { category: record.category
      , subcategory: record.subcategory
      , species: meta.species
      }
  in
    D.div
      [ DA.klass_ ("inventory-item-card " <> className) ]
      [ D.div [ DA.klass_ "item-header" ]
          [ D.div []
              [ D.div [ DA.klass_ "item-brand" ] [ text_ record.brand ]
              , D.div [ DA.klass_ "item-name" ] [ text_ ("'" <> record.name <> "'") ]
              ]
          , D.div [ DA.klass_ "item-img" ]
              [ D.img [ DA.alt_ "product image", DA.src_ meta.img ] [] ]
          ]
      , D.div [ DA.klass_ "item-category" ]
          [ text_ (show record.category <> " - " <> record.subcategory) ]
      , D.div [ DA.klass_ "item-species" ]
          [ text_ ("Species: " <> show meta.species) ]
      , D.div [ DA.klass_ "item-strain_lineage" ]
          [ text_ ("Strain: " <> meta.strain) ]
      , D.div [ DA.klass_ "item-price" ]
          [ text_ ("$" <> show record.price <> " (" <> record.per_package <> "" <> record.measure_unit <> ")") ]
      , D.div [ DA.klass_ "item-quantity" ]
          [ text_ ("in stock: " <> show record.quantity) ]
      ]

-- | Helper functions
generateClassName :: { category :: ItemCategory, subcategory :: String, species :: Species } -> String
generateClassName item =
  "species-" <> toClassName (show item.species)
    <> " category-"
    <> toClassName (show item.category)
    <> " subcategory-"
    <> toClassName item.subcategory

toClassName :: String -> String
toClassName str = toLower (replace (Pattern " ") (Replacement "-") str)

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
      Just { head: priority, tail: rest } ->
        case compareByField priority of
          EQ -> compareWithPriority rest
          result -> result
  in
    compareWithPriority config.sortFields

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT

runLiveView :: Effect Unit
runLiveView = do
  Console.log "Starting MenuLiveView with backend integration"
  liveView