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
import Deku.DOM.Combinators (runOn_)
import Deku.DOM.Listeners (runOn)
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useDynAtBeginning, useRef, useState, (<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import FRP.Event (makeEvent, subscribe)
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
  state <#~> \currentValue ->
    D.input
      [ DA.value_ currentValue
      , DA.placeholder_ placeholderText
      , DL.input_ \evt -> do
          let inputEl = HTMLInput.fromEventTarget =<< target evt
          for_ inputEl \el -> do
            text <- HTMLInput.value el
            if guardEmpty
              then guardAgainstEmpty setter text
              else setter text
      , DA.klass_ inputClass
      ]
      []

renderIntInputField :: String -> Poll Int -> (Int -> Effect Unit) -> (String -> Int) -> Nut
renderIntInputField placeholderText state setter parseFunc = Deku.do
  state <#~> \currentIntValue ->
    D.input
      [ DA.value_ (show currentIntValue)
      , DA.placeholder_ placeholderText
      , DL.input_ \evt -> do
          let inputEl = HTMLInput.fromEventTarget =<< target evt
          for_ inputEl \el -> do
            text <- HTMLInput.value el
            setter (parseFunc text)
      , DA.klass_ inputClass
      ]
      []

renderNumberInputField :: String -> Poll Number -> (Number -> Effect Unit) -> (String -> Number) -> Nut
renderNumberInputField placeholderText state setter parseFunc = Deku.do
  state <#~> \currentValue ->
    D.input
      [ DA.value_ (show currentValue)
      , DA.placeholder_ placeholderText
      , DL.input_ \evt -> do
          let inputEl = HTMLInput.fromEventTarget =<< target evt
          for_ inputEl \el -> do
            text <- HTMLInput.value el
            setter (parseFunc text)
      , DA.klass_ inputClass
      ]
      []

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
renderForm { name, setName, brand, setBrand, price, setPrice, quantity, setQuantity, description, setDescription, handleSubmit } = Deku.do
  nameField <- pure $ renderTextInputField "Name" name setName true
  brandField <- pure $ renderTextInputField "Brand" brand setBrand true
  priceField <- pure $ renderNumberInputField "Price" price setPrice parseNumber
  quantityField <- pure $ renderIntInputField "Quantity" quantity setQuantity parseInt
  descriptionField <- pure $ renderTextInputField "Description" description setDescription false
  
  let
    submitButton :: Nut
    submitButton = D.button
      [ DA.klass_ "p-2 bg-green-500 text-white rounded-md"
      , runOn_ DL.click (liftEffect handleSubmit)
      ]
      [ text_ "Create Item" ]

  D.div_
    [ nameField
    , brandField
    , priceField
    , quantityField
    , descriptionField
    , submitButton
    ]

app :: Effect Unit
app = Deku.do
  setName /\ namePoll <- useState ""
  setBrand /\ brandPoll <- useState ""
  setPrice /\ pricePoll <- useState 0.0
  setQuantity /\ quantityPoll <- useState 0
  setDescription /\ descriptionPoll <- useState ""

  let
    handleSubmit :: Effect Unit
    handleSubmit = do
      nameValue <- namePoll  -- Could not match type Poll with type Effect
      brandValue <- brandPoll
      priceValue <- pricePoll
      quantityValue <- quantityPoll
      descriptionValue <- descriptionPoll

      if nameValue == "" || brandValue == "" || priceValue == 0.0 || quantityValue == 0
        then liftEffect $ window >>= alert "All fields must be filled."
        else do
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
          liftEffect $ log ("New Item: " <> show newItem)
          liftEffect $ window >>= alert "New inventory item created successfully!"
  runInBody $ Deku.do
    form <- renderForm
      { name: namePoll
      , setName
      , brand: brandPoll
      , setBrand
      , price: pricePoll
      , setPrice
      , quantity: quantityPoll
      , setQuantity
      , description: descriptionPoll
      , setDescription
      , handleSubmit
      }
    pure form