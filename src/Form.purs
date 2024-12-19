module Form where

import Prelude

import BudView (ItemCategory(..))

import Data.Array (all, (!!))
import Data.Array (length) as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Num
import Data.String (Pattern(..), Replacement(..), replaceAll, split, take, trim)
import Data.String (length) as String
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Effect (Effect)
import FRP.Poll (Poll)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value) as Input
import Web.HTML.HTMLSelectElement (fromEventTarget, value) as Select
import Web.UIEvent.KeyboardEvent (toEvent)

-- Enhanced Validation Rules
type ValidationRule = String -> Boolean

-- Validation result type
data ValidationResult a = 
  ValidationSuccess a 
  | ValidationError String

-- Type class for converting from form string to target type
class FormValue a where
  fromFormValue :: String -> ValidationResult a

instance formValueString :: FormValue String where
  fromFormValue = ValidationSuccess <<< trim

instance formValueNumber :: FormValue Number where
  fromFormValue str = case Number.fromString (trim str) of
    Just n -> ValidationSuccess n
    Nothing -> ValidationError "Invalid number format"

instance formValueInt :: FormValue Int where
  fromFormValue str = case Int.fromString (trim str) of
    Just n -> ValidationSuccess n
    Nothing -> ValidationError "Invalid integer format"

-- Helper to convert string arrays
cleanStringArray :: Array String -> Array String
cleanStringArray = filter (not <<< null) <<< map trim

-- MenuItemInput type that matches form fields but with proper validation
type ValidatedMenuItemInput = 
  { name :: ValidationResult String
  , sku :: ValidationResult String
  , brand :: ValidationResult String
  , price :: ValidationResult Number
  , quantity :: ValidationResult Int
  , category :: ValidationResult ItemCategory
  , description :: ValidationResult String
  , tags :: Array String
  , strainLineage :: ValidatedStrainLineageInput
  }

type ValidatedStrainLineageInput = 
  { thc :: ValidationResult String
  , cbg :: ValidationResult String
  , strain :: ValidationResult String
  , creator :: ValidationResult String
  , species :: ValidationResult String
  , dominant_tarpene :: ValidationResult String
  , tarpenes :: Array String
  , lineage :: Array String
  , leafly_url :: String
  , img :: String
  }

-- Function to validate and convert form input
validateMenuItemInput :: MenuItemInput -> ValidatedMenuItemInput
validateMenuItemInput input = 
  { name: fromFormValue input.name
  , sku: fromFormValue input.sku
  , brand: fromFormValue input.brand
  , price: fromFormValue input.price
  , quantity: fromFormValue input.quantity
  , category: validateCategory input.category
  , description: fromFormValue input.description
  , tags: cleanStringArray input.tags
  , strainLineage: validateStrainLineageInput input.strainLineage
  }

validateStrainLineageInput :: StrainLineageInput -> ValidatedStrainLineageInput
validateStrainLineageInput input = 
  { thc: fromFormValue input.thc
  , cbg: fromFormValue input.cbg
  , strain: fromFormValue input.strain
  , creator: fromFormValue input.creator
  , species: fromFormValue input.species
  , dominant_tarpene: fromFormValue input.dominant_tarpene
  , tarpenes: cleanStringArray input.tarpenes
  , lineage: cleanStringArray input.lineage
  , leafly_url: input.leafly_url
  , img: input.img
  }

-- Function to convert validated input to MenuItem if all validations pass
createValidatedMenuItem :: ValidatedMenuItemInput -> Either String MenuItem
createValidatedMenuItem input = do
  name <- requireSuccess "name" input.name
  sku <- requireSuccess "sku" input.sku
  brand <- requireSuccess "brand" input.brand
  price <- requireSuccess "price" input.price
  quantity <- requireSuccess "quantity" input.quantity
  category <- requireSuccess "category" input.category
  description <- requireSuccess "description" input.description
  
  strainLineage <- validateStrainLineage input.strainLineage
  
  pure $ MenuItem
    { sort: 0
    , sku
    , brand
    , name
    , price
    , measure_unit: "units"
    , per_package: show quantity
    , quantity
    , category
    , subcategory: categoryToString category
    , description
    , tags: input.tags
    , strain_lineage: strainLineage
    }

validateStrainLineage :: ValidatedStrainLineageInput -> Either String StrainLineage
validateStrainLineage input = do
  thc <- requireSuccess "thc" input.thc
  cbg <- requireSuccess "cbg" input.cbg
  strain <- requireSuccess "strain" input.strain
  creator <- requireSuccess "creator" input.creator
  species <- requireSuccess "species" input.species
  dominant_tarpene <- requireSuccess "dominant_tarpene" input.dominant_tarpene
  
  pure $ StrainLineage
    { thc
    , cbg
    , strain
    , creator
    , species
    , dominant_tarpene
    , tarpenes: input.tarpenes
    , lineage: input.lineage
    , leafly_url: input.leafly_url
    , img: input.img
    }

-- Helper to convert ValidationResult to Either
requireSuccess :: forall a. String -> ValidationResult a -> Either String a
requireSuccess field = case _ of
  ValidationSuccess x -> Right x
  ValidationError err -> Left $ field <> ": " <> err

-- Helper to convert category string to ItemCategory
validateCategory :: String -> ValidationResult ItemCategory
validateCategory = case _ of
  "Flower" -> ValidationSuccess Flower
  "PreRolls" -> ValidationSuccess PreRolls
  "Vaporizers" -> ValidationSuccess Vaporizers
  "Edibles" -> ValidationSuccess Edibles
  "Drinks" -> ValidationSuccess Drinks
  "Concentrates" -> ValidationSuccess Concentrates
  "Topicals" -> ValidationSuccess Topicals
  "Tinctures" -> ValidationSuccess Tinctures
  "Accessories" -> ValidationSuccess Accessories
  _ -> ValidationError "Invalid category"

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

alphanumeric :: ValidationRule
alphanumeric str = case regex "^[A-Za-z0-9-\\s]+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

url :: ValidationRule
url str = case regex "^(https?:\\/\\/)?[\\w\\-]+(\\.[\\w\\-]+)+[/#?]?.*$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

percentage :: ValidationRule
percentage str = case regex "^\\d{1,3}(\\.\\d{1,2})?%$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

dollarAmount :: ValidationRule 
dollarAmount str = case Num.fromString str of
  Just n -> n >= 0.0
  Nothing -> false

maxLength :: Int -> ValidationRule
maxLength n str = String.length str <= n

-- Validation rule combinator
allOf :: Array ValidationRule -> ValidationRule
allOf rules str = all (\rule -> rule str) rules

-- Field Configuration Types
type FieldConfig = 
  { label :: String
  , placeholder :: String
  , validation :: ValidationRule
  , errorMessage :: String
  , formatInput :: String -> String
  }

type DropdownConfig = 
  { label :: String
  , options :: Array { value :: String, label :: String }
  , defaultValue :: String
  }

-- UI Components
makeField :: FieldConfig -> (String -> Effect Unit) -> (Maybe Boolean -> Effect Unit) -> Poll (Maybe Boolean) -> Nut
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
            [ text (map (\mValid -> case mValid of 
                Just false -> config.errorMessage
                _ -> "") validEvent)
            ]
        ]
    ]

makeDropdown :: DropdownConfig -> (String -> Effect Unit) -> (Maybe Boolean -> Effect Unit) -> Poll (Maybe Boolean) -> Nut
makeDropdown config setValue setValid validEvent = 
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
                    setValid (Just (v /= ""))
            ]
            (config.options <#> \opt ->
              D.option
                [ DA.value_ opt.value ]
                [ text_ opt.label ]
            )
        , D.span
            [ DA.klass_ "text-red-500 text-xs" ]
            [ text (map (\mValid -> case mValid of 
                Just false -> "Please select an option"
                _ -> "") validEvent)
            ]
        ]
    ]

makeArrayField :: String -> (Array String -> Effect Unit) -> Nut
makeArrayField label setValue = 
  D.div_
    [ D.div 
        [ DA.klass_ "flex items-center gap-2" ]
        [ D.label_
            [ text_ label ]
        , D.input
            [ DA.placeholder_ "Add items (comma-separated)"
            , DL.keyup_ \evt -> do
                for_ 
                  ((target >=> Input.fromEventTarget) (toEvent evt))
                  \inputElement -> do
                    v <- Input.value inputElement
                    -- Convert the input string to an array and update state
                    setValue $ 
                      if v == "" 
                        then []
                        else map trim $ split (Pattern ",") v
            , DA.klass_ inputKls
            ]
            []
        ]
    ]

-- Validation Presets
type ValidationPreset = 
  { validation :: ValidationRule
  , errorMessage :: String
  , formatInput :: String -> String
  }

-- Standard validation presets
requiredText :: ValidationPreset
requiredText =
  { validation: allOf [nonEmpty, alphanumeric]
  , errorMessage: "Required, text only"
  , formatInput: trim
  }

requiredTextWithLimit :: Int -> ValidationPreset
requiredTextWithLimit limit =
  { validation: allOf [nonEmpty, alphanumeric, maxLength limit]
  , errorMessage: "Required, text only (max " <> show limit <> " chars)"
  , formatInput: trim
  }

percentageField :: ValidationPreset
percentageField = 
  { validation: percentage
  , errorMessage: "Required format: XX.XX%"
  , formatInput: trim
  }

moneyField :: ValidationPreset
moneyField =
  { validation: allOf [nonEmpty, dollarAmount]
  , errorMessage: "Required, valid dollar amount"
  , formatInput: formatDollarAmount
  }

numberField :: ValidationPreset
numberField =
  { validation: allOf [nonEmpty, positiveInteger]
  , errorMessage: "Required, positive whole number"
  , formatInput: \str -> fromMaybe str $ map show $ fromString str
  }

-- Field Configuration Generator
makeFieldConfig :: String -> String -> ValidationPreset -> FieldConfig
makeFieldConfig label placeholder preset =
  { label
  , placeholder
  , validation: preset.validation
  , errorMessage: preset.errorMessage
  , formatInput: preset.formatInput
  }

-- Helper Functions
formatDollarAmount :: String -> String
formatDollarAmount str = 
  if str == "" then ""
  else case Num.fromString str of
    Just n -> 
      let 
        fixed = show n
        parts = split (Pattern ".") fixed
      in case Array.length parts of
        1 -> fixed <> ".00"
        2 -> 
          let decimals = fromMaybe "" $ parts !! 1
          in if String.length decimals >= 2 
             then fromMaybe "" (parts !! 0) <> "." <> take 2 decimals
             else fromMaybe "" (parts !! 0) <> "." <> decimals <> "0"
        _ -> str
    Nothing -> str

-- Category dropdown configuration
categoryConfig :: DropdownConfig
categoryConfig = 
  { label: "Category"
  , options: 
      [ { value: "", label: "Select..." }
      , { value: "Flower", label: "Flower" }
      , { value: "PreRolls", label: "Pre-Rolls" }
      , { value: "Vaporizers", label: "Vaporizers" }
      , { value: "Edibles", label: "Edibles" }
      , { value: "Drinks", label: "Drinks" }
      , { value: "Concentrates", label: "Concentrates" }
      , { value: "Topicals", label: "Topicals" }
      , { value: "Tinctures", label: "Tinctures" }
      , { value: "Accessories", label: "Accessories" }
      ]
  , defaultValue: ""
  }

-- Field Configurations using the new system
nameConfig :: FieldConfig
nameConfig = makeFieldConfig "Name" "Enter product name" (requiredTextWithLimit 50)

skuConfig :: FieldConfig
skuConfig = makeFieldConfig "SKU" "Enter SKU" (requiredTextWithLimit 20)

brandConfig :: FieldConfig
brandConfig = makeFieldConfig "Brand" "Enter brand name" (requiredTextWithLimit 30)

priceConfig :: FieldConfig
priceConfig = makeFieldConfig "Price" "Enter price" moneyField

quantityConfig :: FieldConfig
quantityConfig = makeFieldConfig "Quantity" "Enter quantity" numberField

thcConfig :: FieldConfig
thcConfig = makeFieldConfig "THC %" "Enter THC percentage" percentageField

cbgConfig :: FieldConfig
cbgConfig = makeFieldConfig "CBG %" "Enter CBG percentage" percentageField

strainConfig :: FieldConfig
strainConfig = makeFieldConfig "Strain" "Enter strain name" requiredText

creatorConfig :: FieldConfig
creatorConfig = makeFieldConfig "Creator" "Enter creator name" requiredText

speciesConfig :: FieldConfig
speciesConfig = makeFieldConfig "Species" "Enter species" requiredText

descriptionConfig :: FieldConfig
descriptionConfig = makeFieldConfig "Description" "Enter description" requiredText

dominantTarpeneConfig :: FieldConfig
dominantTarpeneConfig = makeFieldConfig "Dominant Terpene" "Enter dominant terpene" requiredText

stringToItemCategory :: String -> ItemCategory
stringToItemCategory = case _ of
  "Flower" -> Flower
  "PreRolls" -> PreRolls
  "Vaporizers" -> Vaporizers
  "Edibles" -> Edibles
  "Drinks" -> Drinks
  "Concentrates" -> Concentrates
  "Topicals" -> Topicals
  "Tinctures" -> Tinctures
  _ -> Accessories

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
