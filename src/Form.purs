module Form
  where

import Prelude

import Data.Array (all, catMaybes, range, (!!), (:))
import Data.Array (length) as Array
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foldable (for_)
import Data.Int (fromString)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Number
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
import Types (ItemCategory(..), MenuItem(..), Species(..), StrainLineage(..), UUID, parseUUID)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value) as Input
import Web.HTML.HTMLSelectElement (fromEventTarget, value) as Select
import Web.UIEvent.KeyboardEvent (toEvent)

-- Form input types (raw form data)
type MenuItemFormInput = 
  { name :: String
  , sku :: String
  , brand :: String
  , price :: String
  , quantity :: String
  , category :: String
  , description :: String
  , tags :: String
  , strainLineage :: StrainLineageFormInput
  }

type StrainLineageFormInput = 
  { thc :: String
  , cbg :: String
  , strain :: String
  , creator :: String
  , species :: String
  , dominant_tarpene :: String
  , tarpenes :: String
  , lineage :: String
  }

instance formValueItemCategory :: FormValue ItemCategory where
  fromFormValue str = case str of
    "Flower" -> ValidationSuccess Flower
    "PreRolls" -> ValidationSuccess PreRolls
    "Vaporizers" -> ValidationSuccess Vaporizers
    "Edibles" -> ValidationSuccess Edibles
    "Drinks" -> ValidationSuccess Drinks
    "Concentrates" -> ValidationSuccess Concentrates
    "Topicals" -> ValidationSuccess Topicals
    "Tinctures" -> ValidationSuccess Tinctures
    "Accessories" -> ValidationSuccess Accessories
    _ -> 
      case fromString str >>= toEnum of
        Just category -> ValidationSuccess category
        Nothing -> ValidationError "Invalid category value"

instance formValueSpecies :: FormValue Species where
  fromFormValue str = case str of
    "Indica" -> ValidationSuccess Indica
    "IndicaDominant" -> ValidationSuccess IndicaDominantHybrid
    "Hybrid" -> ValidationSuccess Hybrid
    "SativaDominant" -> ValidationSuccess SativaDominantHybrid
    "Sativa" -> ValidationSuccess Sativa
    _ -> 
      case fromString str >>= toEnum of
        Just species -> ValidationSuccess species
        Nothing -> ValidationError "Invalid species value"

instance formValueUUID :: FormValue UUID where
  fromFormValue str = case parseUUID (trim str) of
    Just uuid -> ValidationSuccess uuid
    Nothing -> ValidationError "Invalid UUID format"

-- Type conversion type class
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

validateForm :: MenuItemFormInput -> Either String MenuItem
validateForm input = do
  name <- requireValid "Name" $ fromFormValue input.name
  sku <- requireValid "SKU" $ 
    case parseUUID input.sku of
      Just uuid -> ValidationSuccess uuid
      Nothing -> ValidationError "Invalid UUID format"
  brand <- requireValid "Brand" $ fromFormValue input.brand
  price <- requireValid "Price" $ fromFormValue input.price
  quantity <- requireValid "Quantity" $ fromFormValue input.quantity
  category <- requireValid "Category" $ fromFormValue input.category
  description <- requireValid "Description" $ fromFormValue input.description

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
    , subcategory: show category 
    , description
    , tags: parseCommaList input.tags
    , strain_lineage: strainLineage
    }

validateStrainLineage :: StrainLineageFormInput -> Either String StrainLineage
validateStrainLineage input = do
  thc <- requireValid "THC" $ fromFormValue input.thc
  cbg <- requireValid "CBG" $ fromFormValue input.cbg
  strain <- requireValid "Strain" $ fromFormValue input.strain
  creator <- requireValid "Creator" $ fromFormValue input.creator
  species <- requireValid "Species" $ fromFormValue input.species
  dominant_tarpene <- requireValid "Dominant Terpene" $ fromFormValue input.dominant_tarpene

  pure $ StrainLineage
    { thc
    , cbg
    , strain
    , creator
    , species
    , dominant_tarpene
    , tarpenes: parseCommaList input.tarpenes
    , lineage: parseCommaList input.lineage
    , leafly_url: ""
    , img: ""
    }

-- Field Configuration Types
type FieldConfig = 
  { label :: String
  , placeholder :: String
  , defaultValue :: String
  , validation :: ValidationRule
  , errorMessage :: String
  , formatInput :: String -> String
  }

type DropdownConfig = 
  { label :: String
  , options :: Array { value :: String, label :: String }
  , defaultValue :: String
  }

makeField :: FieldConfig -> (String -> Effect Unit) -> (Maybe Boolean -> Effect Unit) -> Poll (Maybe Boolean) -> Nut
makeField config setValue setValid validEvent = 
  D.div_
    [ D.div 
        [ DA.klass_ "flex items-center gap-2" ]
        [ D.label_
            [ text_ config.label ]
        , D.input
            [ DA.placeholder_ config.placeholder
            , DA.value_ config.defaultValue
            , DL.keyup_ \evt -> do
                let targetEvent = toEvent evt
                for_ 
                  (target targetEvent >>= Input.fromEventTarget)
                  \inputElement -> do
                    v <- Input.value inputElement
                    let formatted = config.formatInput v
                    setValue formatted
                    setValid (Just (config.validation formatted))
            , DL.input_ \evt -> do
                for_ 
                  (target evt >>= Input.fromEventTarget)
                  \inputElement -> do
                    v <- Input.value inputElement
                    let formatted = config.formatInput v
                    setValue formatted
                    setValid (Just (config.validation formatted))
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

-- And let's fix skuConfig to use proper function application
makeFieldConfig :: String -> String -> String -> ValidationPreset -> FieldConfig
makeFieldConfig label placeholder defaultValue preset =
  { label
  , placeholder
  , defaultValue
  , validation: preset.validation
  , errorMessage: preset.errorMessage
  , formatInput: preset.formatInput
  }   

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

-- Generic helper function to get all values of a bounded enum
getAllEnumValues :: ∀ a. BoundedEnum a => Bounded a => Array a
getAllEnumValues = 
  let
    enumValues = map toEnum $ range 0 (fromEnum (top :: a))
  in
    catMaybes enumValues

-- Generic function to create dropdown config
makeEnumDropdown :: ∀ a. BoundedEnum a => Bounded a => Show a => 
  { label :: String, enumType :: a } -> DropdownConfig
makeEnumDropdown { label } = 
  { label
  , options: 
      { value: "", label: "Select..." } :
      map (\val -> { value: show val, label: show val }) 
          (getAllEnumValues :: Array a)
  , defaultValue: ""
  }

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
                    setValue $ parseCommaList v
            , DA.klass_ inputKls
            ]
            []
        ]
    ]

-- Configuration for specific types
categoryConfig :: DropdownConfig
categoryConfig = makeEnumDropdown 
  { label: "Category"
  , enumType: (bottom :: ItemCategory)
  }

speciesConfig :: DropdownConfig
speciesConfig = makeEnumDropdown 
  { label: "Species"
  , enumType: (bottom :: Species)
  }

skuConfig :: String -> FieldConfig
skuConfig defaultValue = makeFieldConfig "SKU" "Enter UUID" defaultValue
  { validation: allOf [nonEmpty, validUUID]
  , errorMessage: "Required, must be a valid UUID"
  , formatInput: trim
  }

nameConfig :: String -> FieldConfig
nameConfig defaultValue = makeFieldConfig "Name" "Enter product name" defaultValue
  (requiredTextWithLimit 50)

brandConfig :: String -> FieldConfig
brandConfig defaultValue = makeFieldConfig "Brand" "Enter brand name" defaultValue
  (requiredTextWithLimit 30)

priceConfig :: String -> FieldConfig
priceConfig defaultValue = makeFieldConfig "Price" "Enter price" defaultValue 
  moneyField

quantityConfig :: String -> FieldConfig
quantityConfig defaultValue = makeFieldConfig "Quantity" "Enter quantity" defaultValue 
  numberField

thcConfig :: String -> FieldConfig
thcConfig defaultValue = makeFieldConfig "THC %" "Enter THC percentage" defaultValue 
  percentageField

cbgConfig :: String -> FieldConfig
cbgConfig defaultValue = makeFieldConfig "CBG %" "Enter CBG percentage" defaultValue 
  percentageField

strainConfig :: String -> FieldConfig
strainConfig defaultValue = makeFieldConfig "Strain" "Enter strain name" defaultValue 
  requiredText

creatorConfig :: String -> FieldConfig
creatorConfig defaultValue = makeFieldConfig "Creator" "Enter creator name" defaultValue 
  requiredText

descriptionConfig :: String -> FieldConfig
descriptionConfig defaultValue = makeFieldConfig "Description" "Enter description" defaultValue 
  requiredText

dominantTarpeneConfig :: String -> FieldConfig
dominantTarpeneConfig defaultValue = makeFieldConfig "Dominant Terpene" "Enter dominant terpene" defaultValue 
  requiredText

tagsConfig :: String -> FieldConfig
tagsConfig defaultValue = makeFieldConfig "Tags" "Enter tags (comma-separated)" defaultValue 
  { validation: const true
  , errorMessage: ""
  , formatInput: trim
  }

tarpenesConfig :: String -> FieldConfig
tarpenesConfig defaultValue = makeFieldConfig "Terpenes" "Enter terpenes (comma-separated)" defaultValue 
  { validation: const true
  , errorMessage: ""
  , formatInput: trim
  }

lineageConfig :: String -> FieldConfig
lineageConfig defaultValue = makeFieldConfig "Lineage" "Enter lineage (comma-separated)" defaultValue 
  { validation: const true
  , errorMessage: ""
  , formatInput: trim
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


-- Core validation types
data ValidationResult a = 
  ValidationSuccess a 
  | ValidationError String

-- Basic validation rules
type ValidationRule = String -> Boolean

nonEmpty :: ValidationRule
nonEmpty = (_ /= "")

positiveInteger :: ValidationRule
positiveInteger str = case fromString str of
  Just n -> n > 0
  Nothing -> false

alphanumeric :: ValidationRule
alphanumeric str = case regex "^[A-Za-z0-9-\\s]+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

vowels :: ValidationRule
vowels str = case regex "^[AEIOUYaeiouy\\s]+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

consonants :: ValidationRule
consonants str = case regex "^[BCDFGHJKLMNPQRSTVWXZbcdfghjklmnpqrstvwxz\\s]+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

percentage :: ValidationRule
percentage str = case regex "^\\d{1,3}(\\.\\d{1,2})?%$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

dollarAmount :: ValidationRule 
dollarAmount str = case Number.fromString str of
  Just n -> n >= 0.0
  Nothing -> false

maxLength :: Int -> ValidationRule
maxLength n str = String.length str <= n

validUUID :: ValidationRule
validUUID str = case parseUUID (trim str) of
  Just _ -> true
  Nothing -> false

allOf :: Array ValidationRule -> ValidationRule
allOf rules str = all (\rule -> rule str) rules

-- Helper functions
parseCommaList :: String -> Array String
parseCommaList str = 
  if str == "" 
    then []
    else map trim $ split (Pattern ",") str

formatDollarAmount :: String -> String
formatDollarAmount str = 
  if str == "" then ""
  else case Number.fromString str of
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

-- Validation functions
validateCategory :: String -> ValidationResult ItemCategory
validateCategory = fromFormValue

requireValid :: ∀ a. String -> ValidationResult a -> Either String a
requireValid field = case _ of
  ValidationSuccess x -> Right x
  ValidationError err -> Left $ field <> ": " <> err

-- Field Configuration Types
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