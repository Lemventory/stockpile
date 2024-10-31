module Render
  ( app)
  where

import Prelude

import BudView (Inventory(..), InventoryResponse(..), MenuItem(..), Metadata(..), QueryMode(..), fetchInventory)
import Data.Array (filter, intercalate, sortBy)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), replace, toLower)
import Data.String.Pattern (Replacement(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut, text_)
import Deku.DOM as D
import Deku.DOM.Attributes (klass_)
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
    -- Unwrapping metadata for species comparison
    Metadata meta1 = item1.metadata
    Metadata meta2 = item2.metadata

    -- Helper function to compare two items by a single field and order
    compareByField :: Tuple SortField SortOrder -> Ordering
    compareByField (Tuple sortField sortOrder) =
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

    -- Apply sorting priority list, finding the first non-EQ comparison
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
    Metadata meta = item.metadata
  in
  D.div
    [ klass_ ("inventory-item-card " <> generateClassName { category: item.category, subcategory: item.subcategory, species: meta.species }) ]
    [ D.div [ klass_ "item-name" ] [ text_ ("'" <> item.name <> "'") ]
    , D.div [ klass_ "item-brand" ] [ text_ ("Brand: " <> item.brand) ]
    , D.div [ klass_ "item-category" ] [ text_ (item.category <> " - " <> item.subcategory) ]
    -- , D.div [ klass_ "item-species" ] [ text_ ("Species: " <> meta.species) ]
    -- , D.div [ klass_ "item-description" ] [ text_ ("Description: " <> item.description) ]
    , D.div [ klass_ "item-price" ] [ text_ ("$" <> show item.price) ]
    , D.div [ klass_ "item-quantity" ] [ text_ ("x" <> show item.quantity) ]
    , D.div [ klass_ "item-tags" ] [ text_ ("" <> intercalate ", " item.tags) ]
    , D.div [ klass_ "item-metadata" ] 
        [ text_ ("Species: " <> meta.species
                 <> ", Strain: " <> meta.strain 
                 <> ", THC: " <> meta.thc 
                 <> ", CBD: " <> meta.cbd) 
        ]
    ]

generateClassName :: { category :: String, subcategory :: String, species :: String } -> String
generateClassName item =
  "species-" <> toClassName item.species <> 
  " category-" <> toClassName item.category <> 
  " subcategory-" <> toClassName item.subcategory

-- Helper function to convert strings to lowercase and replace spaces with hyphens
toClassName :: String -> String
toClassName str = toLower (replace (Pattern " ") (Replacement "-") str)

app :: Effect Unit
app = do
  setInventory /\ inventory <- useState (Inventory [])
  let
    config =
      { sortFields: [ Tuple SortByCategory Ascending, Tuple SortBySpecies Ascending, Tuple SortByName Ascending ]
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