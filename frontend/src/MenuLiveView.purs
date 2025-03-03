module MenuLiveView where

import Prelude

import Data.Array (filter, length, sortBy)
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners (load_) as DL
import Deku.Hooks ((<#~>))
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import FRP.Poll (Poll)
import Types (Inventory(..), MenuItem(..), StrainLineage(..))
import Types.LiveViewConfig (LiveViewConfig, defaultViewConfig)
import Utils (compareMenuItems, generateClassName)

createMenuLiveView :: Poll Inventory -> Poll Boolean -> Poll String -> Nut
createMenuLiveView inventoryPoll loadingPoll errorPoll =
  D.div
    [ DA.klass_ "page-container"
    , DL.load_ \_ -> do
        liftEffect $ Console.log "LiveView component mounting..."
    ]
    [ D.div
        [ DA.klass_ "status-container" ]
        [ loadingPoll <#~> \isLoading ->
            if isLoading then
              D.div [ DA.klass_ "loading-indicator" ]
                [ text_ "Loading data..." ]
            else
              D.div_ []
        , errorPoll <#~> \error ->
            if error /= "" then
              D.div [ DA.klass_ "error-message" ]
                [ text_ error ]
            else
              D.div_ []
        ]
    , D.div
        [ DA.klass_ "inventory-container" ]
        [ inventoryPoll <#~> \inventory ->
            renderInventory defaultViewConfig inventory
        ]
    ]

renderInventory :: LiveViewConfig -> Inventory -> Nut
renderInventory config inventory@(Inventory items) =
  let
    filteredItems =
      if config.hideOutOfStock then filter
        (\(MenuItem item) -> item.quantity > 0)
        items
      else items

    sortedItems = sortBy (compareMenuItems config) filteredItems
  in
    D.div
      [ DA.klass_ "inventory-grid" ]
      [ D.div [ DA.klass_ "inventory-stats" ]
          [ text $ pure $ "Total items: " <> show (length items) ]
      , if length items == 0 then
          D.div [ DA.klass_ "empty-inventory" ]
            [ text_ "No items in inventory" ]
        else
          D.div [ DA.klass_ "inventory-items" ]
            (map renderItem sortedItems)
      ]

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
              , D.div [ DA.klass_ "item-name" ]
                  [ text_ ("'" <> record.name <> "'") ]
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
          [ text_
              ( "$" <> show record.price <> " (" <> record.per_package <> ""
                  <> record.measure_unit
                  <> ")"
              )
          ]
      , D.div [ DA.klass_ "item-description" ]
          [ text_ ("description: " <> show record.description) ]
      , D.div [ DA.klass_ "item-quantity" ]
          [ text_ ("in stock: " <> show record.quantity) ]
      , D.div [ DA.klass_ "item-actions" ]
          [ D.a
              [ DA.klass_ "edit-button"
              , DA.href_ ("/#/edit/" <> show record.sku)
              ]
              [ text_ "Edit" ]
          , D.span
              [ DA.klass_ "text-gray-300 mx-2" ]
              [ text_ "|" ]
          , D.a
              [ DA.klass_ "delete-button text-red-500 hover:text-red-700"
              , DA.href_ ("/#/delete/" <> show record.sku)
              ]
              [ text_ "Delete" ]
          ]
      ]