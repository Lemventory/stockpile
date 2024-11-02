module Inventory (app) where

import Prelude

import BudView (Inventory(..), ItemCategory(..), MenuItem(..), QueryMode(..), StrainLineage(..), itemCategoryToString)
import Data.Array (snoc)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut(..))
import Deku.DOM (HTMLInputElement)
import Deku.DOM as D
import Deku.DOM.Attributes (for_)
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.DOM.Self as Self
import Deku.Hooks (useState')
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Class.Console (log)
import Parsing (runParser)
import Parsing.String.Basic (number, intDecimal)
import Web.DOM.CharacterData (fromEventTarget)
import Web.HTML (window)
import Web.HTML.HTMLInputElement (value)
import Web.HTML.Window (alert)
import Web.UIEvent.KeyboardEvent (toEvent)

-- Initial form state for a new MenuItem
type NewMenuItemForm =
  { brand :: String
  , name :: String
  , price :: Number
  , measure_unit :: String
  , per_package :: String
  , quantity :: Int
  , category :: ItemCategory
  , subcategory :: String
  , description :: String
  , img :: String
  }

emptyForm :: NewMenuItemForm
emptyForm = 
  { brand: ""
  , name: ""
  , price: 0.0
  , measure_unit: ""
  , per_package: ""
  , quantity: 0
  , category: Flower
  , subcategory: ""
  , description: ""
  , img: ""
  }

toMenuItem :: NewMenuItemForm -> MenuItem
toMenuItem form = MenuItem
  { sort: 0
  , sku: "SKU-NEW"
  , brand: form.brand
  , name: form.name
  , price: form.price
  , measure_unit: form.measure_unit
  , per_package: form.per_package
  , quantity: form.quantity
  , category: form.category
  , subcategory: form.subcategory
  , description: form.description
  , tags: []
  , strain_lineage: StrainLineage
      { thc: "0%"
      , cbg: "0%"
      , strain: "Unknown"
      , creator: "Unknown"
      , species: "Indica"
      , dominant_tarpene: ""
      , tarpenes: []
      , lineage: []
      , leafly_url: ""
      , img: form.img
      }
  }

-- Render a text input field with reference
renderInput :: String -> String -> (String -> Effect Unit) -> Maybe (HTMLInputElement -> Effect Unit) -> (HTMLInputElement -> Effect Unit) -> Nut
renderInput placeholderText initialValue onChange inputRef setRef =
  D.div_
    [ D.label_ [ DA.for_ placeholderText ] [ text_ placeholderText ]
    , D.input
        [ DA.value_ initialValue
        , DL.input $ \evt -> case fromEventTarget (toEvent evt) of
            Just target -> value target >>= onChange
            Nothing -> pure unit
        , Self.selfT_ setRef  -- Attach reference
        ]
        []
    ]

-- Render an input field with parsing
renderInputWithParsing :: forall a. String -> String -> (String -> a) -> (a -> Effect Unit) -> Nut
renderInputWithParsing placeholderText initialValue parse onChange =
  D.div_
    [ D.label_ [ DA.for_ placeholderText ] [ text_ placeholderText ]
    , D.input
        [ DA.value_ initialValue
        , DL.input $ \evt -> case fromEventTarget (toEvent evt) of
            Just target -> value target >>= onChange <<< parse
            Nothing -> pure unit
        ]
        []
    ]

-- Parsing helpers
readNumber :: String -> Number
readNumber str = either (const 0.0) identity (runParser str number)

readInt :: String -> Int
readInt str = either (const 0) identity (runParser str intDecimal)

parseCategory :: String -> ItemCategory
parseCategory str = case toLower str of
  "flower" -> Flower
  "prerolls" -> PreRolls
  "vaporizers" -> Vaporizers
  "edibles" -> Edibles
  "drinks" -> Drinks
  "concentrates" -> Concentrates
  "topicals" -> Topicals
  "tinctures" -> Tinctures
  "accessories" -> Accessories
  _ -> Flower


app :: Effect Unit
app = runInBody do
  setInventory /\ inventory <- useState' (Inventory [])
  setForm /\ formData <- useState' emptyForm
  setBrandInput /\ brandInput <- useState' Nothing
  setNameInput /\ nameInput <- useState' Nothing

  let
    guardAgainstEmpty :: String -> String -> Effect Unit
    guardAgainstEmpty fieldName val = 
      when (val == "") do
        window >>= alert (fieldName <> " cannot be empty")

    validateAndAddItem :: Effect Unit
    validateAndAddItem = do
      traverse_ (guardAgainstEmpty "Brand") (formData.brand)
      traverse_ (guardAgainstEmpty "Name") (formData.name)
      let newItem = toMenuItem formData
      setInventory (Inventory (snoc inventory.items newItem))
      setForm emptyForm

  D.div_
    [ renderInput "Brand" formData.brand (\text -> setForm (formData { brand = text })) brandInput setBrandInput
    , renderInput "Name" formData.name (\text -> setForm (formData { name = text })) nameInput setNameInput
    , renderInputWithParsing "Price" (show formData.price) readNumber (\val -> setForm (formData { price = val }))
    , renderInput "Measure Unit" formData.measure_unit (\text -> setForm (formData { measure_unit = text }))
    , renderInput "Per Package" formData.per_package (\text -> setForm (formData { per_package = text }))
    , renderInputWithParsing "Quantity" (show formData.quantity) readInt (\val -> setForm (formData { quantity = val }))
    , renderInput "Category" (itemCategoryToString formData.category) (\text -> setForm (formData { category = parseCategory text }))
    , renderInput "Subcategory" formData.subcategory (\text -> setForm (formData { subcategory = text }))
    , renderInput "Description" formData.description (\text -> setForm (formData { description = text }))
    , renderInput "Image URL" formData.img (\text -> setForm (formData { img = text }))
    , D.button
        [ DL.click_ \_ -> validateAndAddItem ]
        [ text_ "Add Item" ]
    ]