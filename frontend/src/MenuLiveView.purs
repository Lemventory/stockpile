module MenuLiveView where

import Prelude

import API (fetchInventory)
import Data.Array (filter, length, sortBy)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners (load_) as DL
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Timer (setInterval)
import Types (Inventory(..), InventoryResponse(..), MenuItem(..), StrainLineage(..))
import Types.LiveViewConfig (LiveViewConfig, defaultViewConfig)
import Utils (compareMenuItems, generateClassName)

runLiveView :: Nut
runLiveView = Deku.do
  setInventory /\ inventory <- useState (Inventory [])
  setLoading /\ loading <- useState true
  setError /\ error <- useState ""

  D.div
    [ DA.klass_ "page-container" ]
    [ D.div
        [ DA.klass_ "load-container"
        , DL.load_ \_ -> do
            liftEffect $ Console.log "LiveView component mounting..."
            liftEffect $ Console.log $ "Using config mode: " <> show defaultViewConfig.mode
            liftEffect $ Console.log $ "Using API endpoint: " <> defaultViewConfig.fetchConfig.apiEndpoint
            liftEffect $ Console.log $ "Using refresh rate: " <> show defaultViewConfig.refreshRate

            let
              fetchData = do
                liftEffect $ Console.log "Starting data fetch..."
                liftEffect $ setLoading true
                liftEffect $ setError ""
                result <- fetchInventory defaultViewConfig.fetchConfig defaultViewConfig.mode
                liftEffect $ case result of
                  Left err -> do
                    Console.error $ "Error fetching inventory: " <> err
                    setError err
                    setLoading false
                  Right (InventoryData inv) -> do
                    Console.log $ "Success! Received " <> show (length (case inv of Inventory items -> items)) <> " items"
                    setInventory inv
                    setLoading false
                  Right (Message msg) -> do
                    Console.log $ "Received message: " <> msg
                    setError msg
                    setLoading false

            -- Initial fetch
            void $ launchAff_ fetchData

            -- Set up periodic refresh
            void $ liftEffect $ setInterval defaultViewConfig.refreshRate do
              Console.log "Refreshing inventory data..."
              void $ launchAff_ fetchData
        ]
        []
    , D.div
        [ DA.klass_ "status-container" ]
        [ loading <#~> \isLoading -> D.div_
            [ text_ $ if isLoading then "Loading..." else "" ]
        , error <#~> \err -> D.div_
            [ text_ $ if err /= "" then "Error: " <> err else "" ]
        ]
    , D.div
        [ DA.klass_ "inventory-container" ]
        [ inventory <#~> \inv -> D.div_
            [ renderInventory defaultViewConfig inv ]
        ]
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
      [ D.div_ [ text_ $ "Total items: " <> show (length items) ]
      , D.div_ (map renderItem sortedItems)
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