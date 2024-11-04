module AddItem (app) where

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
renderTextInputField placeholderText state setter guardEmpty = 
  D.input
    [ DA.value_ placeholderText
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
  where
    currentValue = useDynAtBeginning state

renderNumberInputField :: String -> Poll Number -> (Number -> Effect Unit) -> (String -> Number) -> Nut
renderNumberInputField placeholderText state setter parseFunc = 
  D.input
    [ DA.value_ (show currentValue)  -- directly use show without `text_`
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
  where
    currentValue = useDynAtBeginning state

renderIntInputField :: String -> Poll Int -> (Int -> Effect Unit) -> (String -> Int) -> Nut
renderIntInputField placeholderText state setter parseFunc = 
  D.input
    [ DA.value_ (show currentIntValue)  -- Ensure this is a String from `Int`
    , DA.placeholder_ placeholderText
    , DL.input_ (\evt -> do
        for_ ((target evt) >>= HTMLInput.fromEventTarget) (\inputEl -> do
          text <- HTMLInput.value inputEl
          setter (parseFunc text)  -- Apply `parseFunc` to convert `String` input
        )
      )
    , DA.klass_ inputClass
    ]
    []
  where
    currentIntValue = useDynAtBeginning state  -- Obtain the `Int` value directly

renderForm :: 
  { name :: Poll String
  , setName :: String -> Effect Unit
  , brand :: Poll String
  , setBrand :: String -> Effect Unit
  , price :: Poll Number
  , setPrice :: Number -> Effect Unit
  , quantity :: Poll Int
  , setQuantity :: Int -> Effect Unit
  , description :: Poll String
  , setDescription :: String -> Effect Unit
  , handleSubmit :: Effect Unit
  } -> Nut
renderForm { name, setName, brand, setBrand, price, setPrice, quantity, setQuantity, description, setDescription, handleSubmit } =
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
app = do
  -- Initialize state and references
  setName /\ namePoll <- useState ""
  setBrand /\ brandPoll <- useState ""
  setPrice /\ pricePoll <- useState 0.0
  setQuantity /\ quantityPoll <- useState 0
  setDescription /\ descriptionPoll <- useState ""

  -- Define the submit handler
  let
    handleSubmit :: Effect Unit
    handleSubmit = do
      nameValue <- namePoll
      brandValue <- brandPoll
      priceValue <- pricePoll
      quantityValue <- quantityPoll
      descriptionValue <- descriptionPoll
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

  -- Unwrap Poll values at the top level using Deku.do and pass them to renderForm
  runInBody $ Deku.do
    name <- useDynAtBeginning namePoll
    brand <- useDynAtBeginning brandPoll
    price <- useDynAtBeginning pricePoll
    quantity <- useDynAtBeginning quantityPoll
    description <- useDynAtBeginning descriptionPoll

    renderForm
      { name
      , setName
      , brand
      , setBrand
      , price
      , setPrice
      , quantity
      , setQuantity
      , description
      , setDescription
      , handleSubmit
      }