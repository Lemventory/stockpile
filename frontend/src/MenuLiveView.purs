module MenuLiveView
  ( runLiveView
  ) where

import Prelude

import Data.Array (filter, length, sortBy)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), replace, toLower)
import Data.String.Pattern (Replacement(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.Effect (useState)
import Deku.Hooks ((<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log) as Console
import Effect.Now (now)
import Fetch (Method(..), fetch)
import Fetch.Yoga.Json (fromJSON)
import Foreign (Foreign)
import Types (Inventory(..), InventoryResponse(..), ItemCategory, MenuItem(..), StrainLineage(..), Species)
import Yoga.JSON (unsafeStringify, writeImpl)

-- | Data types
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

data QueryMode = JsonMode | HttpMode

type Config =
  { sortFields :: Array (Tuple SortField SortOrder)
  , hideOutOfStock :: Boolean
  , mode :: QueryMode
  , refreshRate :: Int
  , screens :: Int
  }

-- | Sorting functions
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
      Just { head: priority, tail: rest } ->
        case compareByField priority of
          EQ -> compareWithPriority rest
          result -> result
  in
    compareWithPriority config.sortFields

-- | Data fetching
fetchInventory :: QueryMode -> Aff (Either String InventoryResponse)
fetchInventory = case _ of
  JsonMode -> fetchInventoryFromJson
  HttpMode -> fetchInventoryFromHttp

fetchInventoryFromJson :: Aff (Either String InventoryResponse)
fetchInventoryFromJson = do
  result <- attempt do
    timestamp <- liftEffect $ show <$> now
    let url = "./inventory.json?t=" <> timestamp
    liftEffect $ Console.log ("Fetching URL: " <> url)
    coreResponse <- fetch url {}
    liftEffect $ Console.log "Response received"
    inventory <- fromJSON coreResponse.json :: Aff Inventory
    pure inventory

  pure case result of
    Left err -> Left $ "Fetch error: " <> show err
    Right inventory -> Right $ InventoryData inventory

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

  pure case result of
    Left err -> Left $ "Fetch error: " <> show err
    Right msg -> Right $ Message msg

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
              [ D.div [ DA.klass_ "item-brand" ] [ D.text_ record.brand ]
              , D.div [ DA.klass_ "item-name" ] [ D.text_ ("'" <> record.name <> "'") ]
              ]
          , D.div [ DA.klass_ "item-img" ]
              [ D.img [ DA.alt_ "product image", DA.src_ meta.img ] [] ]
          ]
      , D.div [ DA.klass_ "item-category" ]
          [ D.text_ (show record.category <> " - " <> record.subcategory) ]
      , D.div [ DA.klass_ "item-species" ]
          [ D.text_ ("Species: " <> show meta.species) ]
      , D.div [ DA.klass_ "item-strain_lineage" ]
          [ D.text_ ("Strain: " <> meta.strain) ]
      , D.div [ DA.klass_ "item-price" ]
          [ D.text_ ("$" <> show record.price <> " (" <> record.per_package <> "" <> record.measure_unit <> ")") ]
      , D.div [ DA.klass_ "item-quantity" ]
          [ D.text_ ("in stock: " <> show record.quantity) ]
      ]

generateClassName :: { category :: ItemCategory, subcategory :: String, species :: Species } -> String
generateClassName item =
  "species-" <> toClassName (show item.species)
    <> " category-"
    <> toClassName (show item.category)
    <> " subcategory-"
    <> toClassName item.subcategory

toClassName :: String -> String
toClassName str = toLower (replace (Pattern " ") (Replacement "-") str)

-- | Main view
liveView :: Effect Unit
liveView = do
  setInventory /\ inventory <- useState (Inventory [])
  setLogging /\ logging <- useState ""

  let
    config =
      { sortFields:
          [ SortByCategory /\ Ascending
          , SortBySpecies /\ Descending
          , SortByQuantity /\ Descending
          ]
      , hideOutOfStock: false
      , mode: JsonMode
      , refreshRate: 5000
      , screens: 1
      }

    fetchAndUpdateInventory :: Effect Unit
    fetchAndUpdateInventory = launchAff_ do
      result <- fetchInventory config.mode
      liftEffect $ case result of
        Left err -> do
          Console.log ("Error fetching inventory: " <> err)
          setLogging err
        Right (InventoryData inv@(Inventory items)) -> do
          let msg = "Received " <> show (length items) <> " items"
          Console.log msg
          for_ items \item ->
            Console.log $ "Item: " <> show item
          setLogging msg
          setInventory inv
        Right (Message msg) -> do
          Console.log msg
          setLogging msg

  -- Initial fetch
  void fetchAndUpdateInventory

  -- Run Deku UI
  void $ runInBody Deku.do
    D.div []
      [ D.div
          [ DA.klass_ "debug-log" ]
          [ logging <#~> D.text_ ]
      , D.div
          [ DA.klass_ "inventory-container" ]
          [ inventory <#~> renderInventory config ]
      ]

runLiveView :: Effect Unit
runLiveView = do
  Console.log "Starting MenuLiveView"
  liveView