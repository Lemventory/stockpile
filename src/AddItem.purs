module AddItem (app) where

import Prelude

import BudView (ItemCategory(..), MenuItem(..), StrainLineage(..))
import Data.Array (intercalate, null)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut, useRant)
import Deku.DOM (HTMLInputElement)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Combinators (runOn_)
import Deku.DOM.Listeners (runOn)
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Effect (useState)
import Deku.Hooks (useDynAtBeginning, useRef, (<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import FRP.Event (Event, makeEvent, sampleOnRight, subscribe)
import FRP.Poll (Poll(..), sample)
import Parsing (runParser)
import Parsing.String.Basic (intDecimal, number)
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLInputElement (fromEventTarget, value) as HTMLInput
import Web.HTML.Window (alert)

-- Constants
inputClass :: String
inputClass = "border border-gray-300 rounded-md p-2 mb-2"

-- Parsing helpers
parseNumber :: String -> Number
parseNumber str = either (const 0.0) identity (runParser str number)

parseInt :: String -> Int
parseInt str = either (const 0) identity (runParser str intDecimal)

-- Form Field Rendering Helpers
renderTextInputField ::
  String -> Poll String -> (String -> Effect Unit) -> Boolean -> Nut
renderTextInputField placeholder state setter guardEmpty = Deku.do
  state <#~> \currentValue ->
    D.input
      [ DA.value_ currentValue
      , DA.placeholder_ placeholder
      , DL.input_ \evt -> do
          let inputEl = HTMLInput.fromEventTarget =<< target evt
          for_ inputEl \el -> do
            text <- HTMLInput.value el
            if guardEmpty && text == ""
              then window >>= alert "This field cannot be empty"
              else setter text
      , DA.klass_ inputClass
      ]
      []

renderIntInputField ::
  String -> Poll Int -> (Int -> Effect Unit) -> (String -> Int) -> Nut
renderIntInputField placeholder state setter parseFunc = Deku.do
  state <#~> \currentIntValue ->
    D.input
      [ DA.value_ (show currentIntValue)
      , DA.placeholder_ placeholder
      , DL.input_ \evt -> do
          let inputEl = HTMLInput.fromEventTarget =<< target evt
          for_ inputEl \el -> do
            text <- HTMLInput.value el
            setter (parseFunc text)
      , DA.klass_ inputClass
      ]
      []

renderNumberInputField ::
  String -> Poll Number -> (Number -> Effect Unit) -> (String -> Number) -> Nut
renderNumberInputField placeholder state setter parseFunc = Deku.do
  state <#~> \currentValue ->
    D.input
      [ DA.value_ (show currentValue)
      , DA.placeholder_ placeholder
      , DL.input_ \evt -> do
          let inputEl = HTMLInput.fromEventTarget =<< target evt
          for_ inputEl \el -> do
            text <- HTMLInput.value el
            setter (parseFunc text)
      , DA.klass_ inputClass
      ]
      []

validateAndCreateMenuItem ::
  { name :: String
  , brand :: String
  , price :: Number
  , quantity :: Int
  , description :: String
  } -> Either (Array String) MenuItem
validateAndCreateMenuItem fields =
  let
    errors = [] `append`
      (if fields.name == "" then ["Name cannot be empty."] else [])
      `append` (if fields.brand == "" then ["Brand cannot be empty."] else [])
      `append` (if fields.price <= 0.0 then ["Price must be greater than 0."] else [])
      `append` (if fields.quantity <= 0 then ["Quantity must be greater than 0."] else [])
  in
    if null errors
      then Right $ MenuItem
        { sort: 0
        , sku: "SKU001"
        , brand: fields.brand
        , name: fields.name
        , price: fields.price
        , measure_unit: "g"
        , per_package: "1"
        , quantity: fields.quantity
        , category: Flower
        , subcategory: ""
        , description: fields.description
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
      else Left errors

handleFormSubmit ::
  Poll String -> Poll String -> Poll Number -> Poll Int -> Poll String -> Poll Unit
handleFormSubmit namePoll brandPoll pricePoll quantityPoll descriptionPoll = do
  name <- sample namePoll
  brand <- sample brandPoll
  price <- sample pricePoll
  quantity <- sample quantityPoll
  description <- sample descriptionPoll
  case validateAndCreateMenuItem { name, brand, price, quantity, description } of
    Left errors -> alertErrors errors
    Right newItem -> logNewItem newItem
  pure unit

-- Function to display errors within the Poll context
alertErrors :: Array String -> Poll Unit
alertErrors errors = do
  let errorMessage = intercalate "\n" errors
  OnlyEvent (makeEvent (\emit -> do
    window >>= \win -> alert win errorMessage
    emit unit
  ))

-- Function to log the new item within the Poll context
logNewItem :: MenuItem -> Poll Unit
logNewItem item = do
  OnlyEvent (makeEvent (\emit -> do
    storeNewItem item
    emit unit
  ))

-- Helper function to store the item, in case `logNewItem` requires external logging or storage
storeNewItem :: MenuItem -> Effect Unit
storeNewItem item = do
  log $ "New item added: " <> show item

-- Form Rendering Function
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
    submitButton = D.button
      [ DA.klass_ "p-2 bg-green-500 text-white rounded-md"
      , runOn_ DL.click handleSubmit
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

-- App Entry Point
app :: Effect Unit
app = do
  setName /\ namePoll <- useState ""
  setBrand /\ brandPoll <- useState ""
  setPrice /\ pricePoll <- useState 0.0
  setQuantity /\ quantityPoll <- useState 0
  setDescription /\ descriptionPoll <- useState ""

  let handleSubmit = handleFormSubmit namePoll brandPoll pricePoll quantityPoll descriptionPoll

  void $ runInBody $ Deku.do
    renderForm
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
