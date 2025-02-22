module MenuLiveView where

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
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import MenuLiveView.DataFetcher (defaultConfig, fetchInventory)
import Types (FetchConfig, Inventory(..), InventoryResponse(..), ItemCategory, MenuItem(..), QueryMode(..), SortField(..), SortOrder(..), Species, StrainLineage(..))

defaultViewConfig
  :: { sortFields :: Array (Tuple SortField SortOrder)
     , hideOutOfStock :: Boolean
     , mode :: QueryMode
     , refreshRate :: Int
     , screens :: Int
     , fetchConfig :: FetchConfig
     }
defaultViewConfig =
  { sortFields:
      [ SortByCategory /\ Ascending
      , SortBySpecies /\ Descending
      , SortByQuantity /\ Descending
      ]
  , hideOutOfStock: true
  , mode: HttpMode
  , refreshRate: 5000
  , screens: 1
  , fetchConfig: defaultConfig
      { apiEndpoint = "http://localhost:8080/inventory"
      , corsHeaders = true
      }
  }

runLiveView :: Nut
runLiveView = Deku.do
  setInventory /\ inventory <- useState (Inventory [])
  setLoading /\ loading <- useState true
  setError /\ error <- useState ""

  let config = defaultViewConfig

  D.div
    [ DA.klass_ "page-container" ]
    [ D.div
        [ DA.klass_ "load-container"
        , DL.load_ \_ -> do
            setLoading true
            launchAff_ do
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
        ]
        []
    , D.div
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

renderInventory
  :: { sortFields :: Array (Tuple SortField SortOrder)
     , hideOutOfStock :: Boolean
     , mode :: QueryMode
     , refreshRate :: Int
     , screens :: Int
     , fetchConfig :: FetchConfig
     }
  -> Inventory
  -> Nut
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

compareMenuItems
  :: { sortFields :: Array (Tuple SortField SortOrder)
     , hideOutOfStock :: Boolean
     , mode :: QueryMode
     , refreshRate :: Int
     , screens :: Int
     , fetchConfig :: FetchConfig
     }
  -> MenuItem
  -> MenuItem
  -> Ordering
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

generateClassName :: { category :: ItemCategory, subcategory :: String, species :: Species } -> String
generateClassName item =
  "species-" <> toClassName (show item.species)
    <> " category-"
    <> toClassName (show item.category)
    <> " subcategory-"
    <> toClassName item.subcategory

toClassName :: String -> String
toClassName str = toLower (replace (Pattern " ") (Replacement "-") str)