module Inventory (app) where

import Prelude

import BudView (ItemCategory(..), MenuItem(..), StrainLineage(..))
import Data.Either (either)
import Data.Foldable (for_)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM (HTMLInputElement)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useDynAtBeginning, useRef, useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Console (log)
import FRP.Poll (Poll)
import Parsing (runParser)
import Parsing.String.Basic (intDecimal, number)
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLInputElement (fromEventTarget, value) as HTMLInput
import Web.HTML.Window (alert)

inputClass :: String
inputClass = "border border-gray-300 rounded-md p-2 mb-2"

parseNumber :: String -> Number
parseNumber str = either (const 0.0) identity (runParser str number)

parseInt :: String -> Int
parseInt str = either (const 0) identity (runParser str intDecimal)

guardAgainstEmpty :: (String -> Effect Unit) -> String -> Effect Unit
guardAgainstEmpty setField text = do
  if text == ""
    then window >>= alert "This field cannot be empty"
    else setField text

renderTextInputField :: String -> Poll String -> (String -> Effect Unit) -> Boolean -> Nut
renderTextInputField placeholderText state setter guardEmpty = Deku.do
  { value: currentValue } <- useDynAtBeginning state
  D.input
    [ DA.value_ currentValue
    , DA.placeholder_ placeholderText
    , DL.input_ (\evt -> do
        for_ ((target evt) >>= HTMLInput.fromEventTarget) (\inputEl -> do
          text <- HTMLInput.value inputEl
          if guardEmpty then guardAgainstEmpty setter text else setter text
        )
      )
    , DA.klass_ inputClass
    ]
    []

renderNumberInputField :: String -> Poll Number -> (Number -> Effect Unit) -> (String -> Number) -> Nut
renderNumberInputField placeholderText state setter parseFunc = Deku.do
  { value: currentValue } <- useDynAtBeginning state
  D.input
    [ DA.value_ (show currentValue)
    , DA.placeholder_ placeholderText
    , DL.input_ (\evt -> do
        for_ ((target evt) >>= HTMLInput.fromEventTarget) (\inputEl -> do
          text <- HTMLInput.value inputEl
          setter (parseFunc text)
        )
      )
    , DA.klass_ inputClass
    ]
    []

renderIntInputField :: String -> Poll Int -> (Int -> Effect Unit) -> (String -> Int) -> Nut
renderIntInputField placeholderText state setter parseFunc = Deku.do
  { value: currentValue } <- useDynAtBeginning state
  D.input
    [ DA.value_ (show currentValue)
    , DA.placeholder_ placeholderText
    , DL.input_ (\evt -> do
        for_ ((target evt) >>= HTMLInput.fromEventTarget) (\inputEl -> do
          text <- HTMLInput.value inputEl
          setter (parseFunc text)
        )
      )
    , DA.klass_ inputClass
    ]
    []

renderForm :: 
  { setName :: String -> Effect Unit
  , name :: Poll String
  , setBrand :: String -> Effect Unit
  , brand :: Poll String
  , setPrice :: Number -> Effect Unit
  , price :: Poll Number
  , setQuantity :: Int -> Effect Unit
  , quantity :: Poll Int
  , setDescription :: String -> Effect Unit
  , description :: Poll String
  , handleSubmit :: Effect Unit
  } -> Nut
renderForm { setName, name, setBrand, brand, setPrice, price, setQuantity, quantity, setDescription, description, handleSubmit } =
  D.div_
    [ D.div
        [ renderTextInputField "Name" name setName true
        , renderTextInputField "Brand" brand setBrand true
        , renderNumberInputField "Price" price setPrice parseNumber
        , renderIntInputField "Quantity" quantity setQuantity parseInt
        , renderTextInputField "Description" description setDescription false
        , D.button
            [ DL.click_ (\_ -> handleSubmit)
            , DA.klass_ "p-2 bg-green-500 text-white rounded-md"
            ]
            [ text_ "Create Item" ]
        ]
    ]

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

  renderForm
    { setName, name
    , setBrand, brand
    , setPrice, price
    , setQuantity, quantity
    , setDescription, description
    , handleSubmit
    }
