module LiveViewModule
  ( runLiveView
  ) where

import Prelude

import API (fetchInventory)
import Data.Array (filter, length, sortBy)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.Effect (useState)
import Deku.Hooks ((<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Types (Inventory(..), InventoryResponse(..), MenuItem(..), StrainLineage(..))
import Types.LiveViewConfig (LiveViewConfig, defaultViewConfig)
import Utils (compareMenuItems, generateClassName)

runLiveView :: Effect Unit
runLiveView = do
  Console.log "Starting MenuLiveView with backend integration"
  liveView

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

  void fetchAndUpdateInventory

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

renderInventory :: LiveViewConfig -> Inventory -> Nut
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