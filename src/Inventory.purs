module Inventory (app) where

import Prelude

import BudView (ItemCategory(..), MenuItem(..), StrainLineage(..))
import Data.Either (Either(..), either)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut, useRef)
import Deku.DOM (HTMLElement)
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
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLInputElement (HTMLInputElement, fromEventTarget, value)
import Web.HTML.HTMLInputElement as HTMLInput
import Web.HTML.Window (alert)
import Web.UIEvent.KeyboardEvent (toEvent)

-- Simple CSS for input styling
inputClass :: String
inputClass = "border border-gray-300 rounded-md p-2 mb-2"

-- Parsing helpers
parseNumber :: String -> Number
parseNumber str = either (const 0.0) identity (runParser str number)

parseInt :: String -> Int
parseInt str = either (const 0) identity (runParser str intDecimal)

guardAgainstEmpty :: (String -> Effect Unit) -> String -> Effect Unit 
guardAgainstEmpty setField text = do
  if text == ""
    then window >>= alert "This field cannot be empty"
    else setField text

-- Main form component to capture user input for creating a new inventory item
app :: Effect Unit
app = runInBody $ Deku.do
  setName /\ name <- useState ""
  nameRef <- useRef "" name
  setBrand /\ brand <- useState ""
  brandRef <- useRef "" brand
  setPrice /\ price <- useState 0.0
  priceRef <- useRef 0.0 price
  setQuantity /\ quantity <- useState 0
  quantityRef <- useRef 0 quantity
  setDescription /\ description <- useState ""
  descriptionRef <- useRef "" description

  let
    handleSubmit :: Effect Unit
    handleSubmit = do
      nameValue <- nameRef
      brandValue <- brandRef
      priceValue <- priceRef
      quantityValue <- quantityRef
      descriptionValue <- descriptionRef
      let newItem = MenuItem
            { sort: 0 
            , sku: "SKU001" 
            , brand: brandValue
            , name: nameValue
            , price: priceValue
            , measure_unit: "g" 
            , per_package: "1"
            , quantity: quantityValue
            , category: Flower
            , subcategory: ""
            , description: descriptionValue
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
      log ("New Item: " <> show newItem)
      window >>= alert "New inventory item created successfully!"

  -- Rendering the input form
  D.div_
    [ D.div
        [ D.input
            [ DA.value_ name
            , DA.placeholder "Name"
            , DL.input \evt -> do
                for_ ((target evt) >>= HTMLInput.fromEventTarget) (guardAgainstEmpty setName)
            , DA.klass_ inputClass
            ]
            []
        , D.input
            [ DA.value_ brand
            , DA.placeholder "Brand"
            , DL.input \evt -> do
                for_ ((target evt) >>= HTMLInput.fromEventTarget) (guardAgainstEmpty setBrand)
            , DA.klass_ inputClass
            ]
            []
        , D.input
            [ DA.value_ (show price)
            , DA.placeholder "Price"
            , DL.input \evt -> do
                for_ ((target evt) >>= HTMLInput.fromEventTarget) \inputEl -> do
                  text <- HTMLInput.value inputEl
                  setPrice (parseNumber text)
            , DA.klass_ inputClass
            ]
            []
        , D.input
            [ DA.value_ (show quantity)
            , DA.placeholder "Quantity"
            , DL.input \evt -> do
                for_ ((target evt) >>= HTMLInput.fromEventTarget) \inputEl -> do
                  text <- HTMLInput.value inputEl
                  setQuantity (parseInt text)
            , DA.klass_ inputClass
            ]
            []
        , D.input
            [ DA.value_ description
            , DA.placeholder "Description"
            , DL.input \evt -> do
                for_ ((target evt) >>= HTMLInput.fromEventTarget) \inputEl -> do
                  text <- HTMLInput.value inputEl
                  setDescription text
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