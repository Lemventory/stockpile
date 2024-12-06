module AddItem where

import Prelude

import Data.Array (all)
import Data.Array (length) as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), Replacement(..), replaceAll, length, split)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, useState')
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value) as Input
import Web.HTML.HTMLSelectElement (fromEventTarget, value) as Select
import Web.UIEvent.KeyboardEvent (toEvent)

-- Validation Rules
type ValidationRule = String -> Boolean

nonEmpty :: ValidationRule
nonEmpty = (_ /= "")

positiveInteger :: ValidationRule
positiveInteger str = case fromString str of
  Just n -> n > 0
  Nothing -> false

textOnly :: ValidationRule
textOnly str = case regex "^[A-Za-z\\s]+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

dollarAmount :: ValidationRule
dollarAmount str = case regex "^\\d+\\.\\d{2}$" noFlags of
  Left _ -> false
  Right validRegex -> 
    if test validRegex str
    then 
      let parts = split (Pattern ".") str
      in Array.length parts == 2
    else false

maxLength :: Int -> ValidationRule
maxLength n str = length str <= n

-- Combine multiple validation rules
allOf :: Array ValidationRule -> ValidationRule
allOf rules str = all (\rule -> rule str) rules

-- Field Configuration
type FieldConfig = 
  { label :: String
  , placeholder :: String
  , validation :: ValidationRule
  , errorMessage :: String
  , formatInput :: String -> String
  }

-- Dropdown Configuration
type DropdownConfig = 
  { label :: String
  , options :: Array { value :: String, label :: String }
  , defaultValue :: String
  }

-- Styling
inputKls :: String
inputKls =
  """rounded-md border-gray-300 shadow-sm
     border-2 mr-2 border-solid
     focus:border-indigo-500 focus:ring-indigo-500
     sm:text-sm"""

buttonClass :: String -> String
buttonClass color =
  replaceAll (Pattern "COLOR") (Replacement color)
    """mb-3 inline-flex items-center rounded-md
       border border-transparent bg-COLOR-600 px-3 py-2
       text-sm font-medium leading-4 text-white shadow-sm
       hover:bg-COLOR-700 focus:outline-none focus:ring-2
       focus:ring-COLOR-500 focus:ring-offset-2"""

app :: Effect Unit
app = void $ runInBody Deku.do
  -- States for each field
  setField1 /\ field1Event <- useState'
  setValid1 /\ valid1Event <- useState (Nothing :: Maybe Boolean)
  
  setField2 /\ field2Event <- useState'
  setValid2 /\ valid2Event <- useState (Nothing :: Maybe Boolean)
  
  setField3 /\ field3Event <- useState'
  setValid3 /\ valid3Event <- useState (Nothing :: Maybe Boolean)

  -- New: State for dropdown
  setDropdown /\ dropdownEvent <- useState'

  let
    -- Text only field with max length
    field1Config :: FieldConfig
    field1Config = 
      { label: "Name"
      , placeholder: "Enter name (letters only)"
      , validation: allOf [nonEmpty, textOnly, maxLength 50]
      , errorMessage: "only letters & spaces (max 50 characters)"
      , formatInput: identity
      }

    -- Positive integer field
    field2Config :: FieldConfig
    field2Config = 
      { label: "Amount"
      , placeholder: "Enter a positive number"
      , validation: allOf [nonEmpty, positiveInteger]
      , errorMessage: "whole numbers only!"
      , formatInput: \str -> fromMaybe str $ map show $ fromString str
      }

    -- Dollar amount field
    field3Config :: FieldConfig
    field3Config = 
      { label: "Price"
      , placeholder: "0.00"
      , validation: allOf [nonEmpty, dollarAmount]
      , errorMessage: "valid dollar amounts only (e.g. 1.99)"
      , formatInput: identity
      }

    -- New: Dropdown configuration
    dropdownConfig :: DropdownConfig
    dropdownConfig = 
      { label: "Category"
      , options: 
          [ { value: "sativa", label: "Sativa" }
          , { value: "indica", label: "Indica" }
          ]
      , defaultValue: "sativa"
      }

    makeField config setValue setValid validEvent = 
      D.div_
        [ D.div 
            [ DA.klass_ "flex items-center gap-2" ]
            [ D.label_
                [ text_ config.label ]
            , D.input
                [ DA.placeholder_ config.placeholder
                , DL.keyup_ \evt -> do
                    for_ 
                      ((target >=> Input.fromEventTarget) (toEvent evt))
                      \inputElement -> do
                        v <- Input.value inputElement
                        let formatted = config.formatInput v
                        setValue formatted
                        setValid (Just (config.validation formatted))
                , DA.value_ ""
                , DA.klass_ inputKls
                ]
                []
            , D.span
                [ DA.klass_ "text-red-500 text-xs" ]
                [ text $ map 
                    (maybe "" \isValid -> if isValid then "" else config.errorMessage) 
                    validEvent
                ]
            ]
        ]

    makeDropdown :: DropdownConfig -> (String -> Effect Unit) -> Nut
    makeDropdown config setValue = 
      D.div_
        [ D.div 
            [ DA.klass_ "flex items-center gap-2" ]
            [ D.label_
                [ text_ config.label ]
            , D.select
                [ DA.klass_ inputKls
                , DL.change_ \evt -> do
                    for_ 
                      (target evt >>= Select.fromEventTarget)
                      \selectElement -> do
                        v <- Select.value selectElement
                        setValue v
                ]
                (config.options <#> \opt ->
                  D.option
                    [ DA.value_ opt.value ]
                    [ text_ opt.label ]
                )
            ]
        ]

    isFormValid = ado
      v1 <- valid1Event
      v2 <- valid2Event
      v3 <- valid3Event
      in (fromMaybe false v1) && (fromMaybe false v2) && (fromMaybe false v3)

    isButtonDisabled = map (not >>> show) isFormValid

  D.div_
    [ D.div
        [ DA.klass_ "space-y-4" ]
        [ makeField field1Config setField1 setValid1 valid1Event
        , makeField field2Config setField2 setValid2 valid2Event
        , makeField field3Config setField3 setValid3 valid3Event
        , makeDropdown dropdownConfig setDropdown
        ]
    , D.button
        [ DA.klass_ $ buttonClass "green"
        , DA.disabled isButtonDisabled
        , DL.click_ \_ -> do
            setField1 ""
            setValid1 Nothing
            setField2 ""
            setValid2 Nothing
            setField3 ""
            setValid3 Nothing
            setDropdown dropdownConfig.defaultValue
        ]
        [ text_ "Submit" ]
    , D.div_ 
        [ text $ map (\val -> "Name: " <> val) field1Event
        , D.br_ []
        , text $ map (\val -> "Amount: " <> val) field2Event
        , D.br_ []
        , text $ map (\val -> "Price: $" <> val) field3Event
        , D.br_ []
        , text $ map (\val -> "Category: " <> val) dropdownEvent
        ]
    ]