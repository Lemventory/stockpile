module AddItem (app) where

import Prelude

import BudView (ItemCategory(..), MenuItem(..), StrainLineage(..))
import Data.Either (either)
import Data.Foldable (for_)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut, useState')
import Deku.DOM (HTMLInputElement)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
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

renderTextInputField :: String -> String -> (String -> Effect Unit) -> Boolean -> Nut
renderTextInputField placeholderText currentValue setter guardEmpty = 
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

renderNumberInputField :: String -> Number -> (Number -> Effect Unit) -> (String -> Number) -> Nut
renderNumberInputField placeholderText currentValue setter parseFunc = 
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

renderIntInputField :: String -> Int -> (Int -> Effect Unit) -> (String -> Int) -> Nut
renderIntInputField placeholderText currentValue setter parseFunc = 
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
  { nameValue :: String
  , setName :: String -> Effect Unit
  , brandValue :: String
  , setBrand :: String -> Effect Unit
  , priceValue :: Number
  , setPrice :: Number -> Effect Unit
  , quantityValue :: Int
  , setQuantity :: Int -> Effect Unit
  , descriptionValue :: String
  , setDescription :: String -> Effect Unit
  , handleSubmit :: Effect Unit
  } -> Nut
renderForm { nameValue, setName, brandValue, setBrand, priceValue, setPrice, quantityValue, setQuantity, descriptionValue, setDescription, handleSubmit } =
  D.div_
    [ D.div
        [ renderTextInputField "Name" nameValue setName true
        , renderTextInputField "Brand" brandValue setBrand true
        , renderNumberInputField "Price" priceValue setPrice parseNumber
        , renderIntInputField "Quantity" quantityValue setQuantity parseInt
        , renderTextInputField "Description" descriptionValue setDescription false
        , D.button
            [ DL.click_ (\_ -> handleSubmit)
            , DA.klass_ "p-2 bg-green-500 text-white rounded-md"
            ]
            [ text_ "Create Item" ]
        ]
    ]

app :: Effect Unit
app = do
  -- Initialize state and references
  setName /\ name <- useState' ""
  setBrand /\ brand <- useState' ""
  setPrice /\ price <- useState' 0.0
  setQuantity /\ quantity <- useState' 0
  setDescription /\ description <- useState' ""

  -- Define the submit handler
  let
    handleSubmit :: Effect Unit
    handleSubmit = do
      nameValue <- name
      brandValue <- brand
      priceValue <- price
      quantityValue <- quantity
      descriptionValue <- description
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

  -- Render the form with Deku.do at the end
  runInBody $ Deku.do
    renderForm
      { nameValue: name
      , setName
      , brandValue: brand
      , setBrand
      , priceValue: price
      , setPrice
      , quantityValue: quantity
      , setQuantity
      , descriptionValue: description
      , setDescription
      , handleSubmit
      }
