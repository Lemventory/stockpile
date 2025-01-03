module Render
  ( app)
  where

import Prelude

import Types (Inventory(..), InventoryResponse(..), ItemCategory, MenuItem(..), QueryMode(..), StrainLineage(..), itemCategoryToString)
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
          SortByCategory -> compare (itemCategoryToString item1.category) (itemCategoryToString item2.category)
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
    [ klass_ ("inventory-item-card " <> generateClassName { category: item.category, subcategory: item.subcategory, species: meta.species }) ]
    [ D.div [ klass_ "item-header" ]
        [ D.div []
            [ D.div [ klass_ "item-brand" ] [ text_ item.brand ]
            , D.div [ klass_ "item-name" ] [ text_ ("'" <> item.name <> "'") ]
            ]
        , D.div [ klass_ "item-img" ] [ img [ alt_ "weed pic", src_ meta.img ] [] ]
        ]
    , D.div [ klass_ "item-category" ] [ text_ (itemCategoryToString item.category <> " - " <> item.subcategory) ]
    , D.div [ klass_ "item-species" ] [ text_ ("Species: " <> meta.species) ]
    , D.div [ klass_ "item-strain_lineage" ] [ text_ ("Strain: " <> meta.strain) ]
    , D.div [ klass_ "item-price" ] [ text_ ("$" <> show item.price <> " (" <> item.per_package <> "" <> item.measure_unit <> ")") ]
    , D.div [ klass_ "item-quantity" ] [ text_ ("in stock: " <> show item.quantity) ]
    ]

generateClassName :: { category :: ItemCategory, subcategory :: String, species :: String } -> String
generateClassName item =
  "species-" <> toClassName item.species <> 
  " category-" <> toClassName (itemCategoryToString item.category) <> 
  " subcategory-" <> toClassName item.subcategory

toClassName :: String -> String
toClassName str = toLower (replace (Pattern " ") (Replacement "-") str)

app :: Effect Unit
app = do
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