module Inventory (app) where

import Prelude

import BudView (ItemCategory(..), MenuItem(..), StrainLineage(..))
import Data.Either (Either(..), either)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM (HTMLInputElement, HTMLElement)
import Deku.DOM as D
import Deku.DOM.Attributes (value)
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.DOM.Self as Self
import Deku.Do as Deku
import Deku.Hooks (useDyn, useState, useState')
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Console (log)
import FRP.Event.Class ((<|*>))
import Parsing (runParser)
import Parsing.String.Basic (intDecimal, number)
import Web.DOM.CharacterData (fromEventTarget)
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.Window (alert)

-- Simple CSS for input styling
inputClass :: String
inputClass = "border border-gray-300 rounded-md p-2 mb-2"

-- Parsing helpers
parseNumber :: String -> Number
parseNumber str = either (const 0.0) identity (runParser str number)

parseInt :: String -> Int
parseInt str = either (const 0) identity (runParser str intDecimal)

-- Handler to check if an input is empty and show an alert if it is
guardAgainstEmpty
  :: (String -> Effect Unit)
  -> HTMLInputElement ()
  -> Effect Unit -- Could not match kind Row Type with kind Type
guardAgainstEmpty setField inputEl = do
  text <- value inputEl
  if text == ""
    then window >>= alert "This field cannot be empty"
    else setField text

-- Main form component to capture user input for creating a new inventory item
app :: Effect Unit
app = runInBody $ Deku.do
  setName /\ name <- useState ""
  setBrand /\ brand <- useState ""
  setCategory /\ category <- useState "Flower" -- default category
  setPrice /\ price <- useState 0.0
  setQuantity /\ quantity <- useState 0
  setDescription /\ description <- useState ""

  -- Submit handler to create and display the new MenuItem
  let
    handleSubmit :: Effect Unit
    handleSubmit = do
      let newItem = MenuItem
            { sort: 0 -- Sort can be configured differently as needed
            , sku: "SKU001" -- SKU can be auto-generated or provided by user
            , brand
            , name
            , price
            , measure_unit: "g" -- This is a sample measure unit
            , per_package: "1"
            , quantity
            , category: Flower -- Convert from category input to ItemCategory
            , subcategory: ""
            , description
            , tags: []
            , strain_lineage: StrainLineage
                { thc: "15%"
                , cbg: "1%"
                , strain: "Sample Strain"
                , creator: "Sample Creator"
                , species: "Hybrid"
                , dominant_tarpene: "Limonene"
                , tarpenes: []
                , lineage: []
                , leafly_url: ""
                , img: ""
                }
            }
      -- Perform validation, then insert the new item
      log ("New Item: " <> show newItem)
      alert "New inventory item created successfully!"

  -- Rendering the input form
  D.div_
    [ D.div
        [ D.input
            [ DA.value_ name
            , DA.placeholder "Name"
            , DL.input \evt -> case fromEventTarget (target evt) of
                Just inputEl -> guardAgainstEmpty setName inputEl
                Nothing -> pure unit
            , DA.klass_ inputClass
            ]
            []
        , D.input
            [ DA.value_ brand
            , DA.placeholder "Brand"
            , DL.input \evt -> case fromEventTarget (target evt) of
                Just inputEl -> guardAgainstEmpty setBrand inputEl
                Nothing -> pure unit
            , DA.klass_ inputClass
            ]
            []
        , D.input
            [ DA.placeholder "Price"
            , DA.value_ (show price)
            , DL.input \evt -> case fromEventTarget (target evt) of
                Just inputEl -> value inputEl >>= setPrice <<< parseNumber
                Nothing -> pure unit
            , DA.klass_ inputClass
            ]
            []
        , D.input
            [ DA.placeholder "Quantity"
            , DA.value_ (show quantity)
            , DL.input \evt -> case fromEventTarget (target evt) of
                Just inputEl -> value inputEl >>= setQuantity <<< parseInt
                Nothing -> pure unit
            , DA.klass_ inputClass
            ]
            []
        , D.input
            [ DA.placeholder "Description"
            , DA.value_ description
            , DL.input \evt -> case fromEventTarget (target evt) of
                Just inputEl -> value inputEl >>= setDescription
                Nothing -> pure unit
            , DA.klass_ inputClass
            ]
            []
        , D.button
            [ DL.click_ \_ -> handleSubmit
            , DA.klass_ "p-2 bg-green-500 text-white rounded-md"
            ]
            [ text_ "Create Item" ]
        ]
    ]