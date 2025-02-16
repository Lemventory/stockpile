module Validation where

import Prelude
import Types (class FieldValidator, class FormValue, FieldConfigRow, HTMLFormField, ItemCategory, MenuItem(..), MenuItemFormInput, NumberFieldConfig, StrainLineage(..), StrainLineageFormInput, TextFieldConfig, ValidationPreset, ValidationResult(..), ValidationRule(..), fromFormValue, runValidation, validationError)

import Data.Array (all)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Number
import Data.String (length) as String
import Data.String (trim)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Type.Proxy (Proxy(..))
import UUID (UUID, parseUUID)
import Utils (formatDollarAmount, parseCommaList)
import Types

-- | Basic validation rules
requireValid :: forall a. String -> ValidationResult a -> Either String a
requireValid field = case _ of
  ValidationSuccess x -> Right x
  ValidationError err -> Left $ field <> ": " <> err

requiredField :: forall a. FormValue a => FieldValidator a => ValidationRule
requiredField = ValidationRule \str ->
  let
    validate :: String -> Either String a
    validate = validateField
  in
    case validate str of
      Right _ -> true
      Left _ -> false

nonEmpty :: ValidationRule
nonEmpty = ValidationRule (_ /= "")

validUUID :: ValidationRule
validUUID = ValidationRule \str -> case parseUUID (trim str) of
  Just _ -> true
  Nothing -> false

alphanumeric :: ValidationRule
alphanumeric = ValidationRule \str -> case regex "^[A-Za-z0-9-\\s]+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

percentage :: ValidationRule
percentage = ValidationRule \str -> case regex "^\\d{1,3}(\\.\\d{1,2})?%$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

dollarAmount :: ValidationRule
dollarAmount = ValidationRule \str -> case Number.fromString str of
  Just n -> n >= 0.0
  Nothing -> false

positiveInteger :: ValidationRule
positiveInteger = ValidationRule \str -> case fromString str of
  Just n -> n > 0
  Nothing -> false

vowels :: ValidationRule
vowels = ValidationRule \str -> case regex "^[AEIOUYaeiouy\\s]+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

consonants :: ValidationRule
consonants = ValidationRule \str -> case regex "^[BCDFGHJKLMNPQRSTVWXZbcdfghjklmnpqrstvwxz\\s]+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

commaList :: ValidationRule
commaList = ValidationRule \str -> case regex "^[^,]+(,[^,]+)*$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

maxLength :: Int -> ValidationRule
maxLength n = ValidationRule \str -> String.length str <= n

allOf :: Array ValidationRule -> ValidationRule
allOf rules = ValidationRule \str ->
  all (\(ValidationRule rule) -> rule str) rules

-- | Validation presets
requiredText :: ValidationPreset
requiredText =
  { validation: allOf [ nonEmpty, alphanumeric ]
  , errorMessage: "Required, text only"
  , formatInput: trim
  }

requiredTextWithLimit :: Int -> ValidationPreset
requiredTextWithLimit limit =
  { validation: allOf [ nonEmpty, alphanumeric, maxLength limit ]
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
  { validation: allOf [ nonEmpty, dollarAmount ]
  , errorMessage: "Required, valid dollar amount"
  , formatInput: formatDollarAmount
  }

numberField :: ValidationPreset
numberField =
  { validation: allOf [ nonEmpty, positiveInteger ]
  , errorMessage: "Required, positive whole number"
  , formatInput: \str -> fromMaybe str $ map show $ fromString str
  }

commaListField :: ValidationPreset
commaListField =
  { validation: commaList
  , errorMessage: "Must be a comma-separated list"
  , formatInput: trim
  }

multilineText :: ValidationPreset
multilineText =
  { validation: nonEmpty
  , errorMessage: "Required"
  , formatInput: identity
  }

-- | Form validation
validateForm :: forall r. Record (FieldConfigRow r) -> MenuItemFormInput -> Either String MenuItem
validateForm _ input = do
  name <- validateTextField
    { label: "Name"
    , maxLength: 50
    , placeholder: "Enter name"
    , defaultValue: ""
    , validation: allOf [ nonEmpty, alphanumeric ]
    , errorMessage: "Required, text only (max 50 chars)"
    , formatInput: trim
    }
    input.name

  sku <- requireValid "SKU" $ fromFormValue input.sku
  brand <- requireValid "Brand" $ fromFormValue input.brand
  price <- requireValid "Price" $ fromFormValue input.price
  quantity <- requireValid "Quantity" $ fromFormValue input.quantity
  category <- requireValid "Category" $ fromFormValue input.category

  strainLineage <- validateStrainLineage input.strain_lineage

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
    , description: input.description
    , tags: parseCommaList input.tags
    , effects: parseCommaList input.effects
    , strain_lineage: strainLineage
    }

validateTextField :: forall r. Record (TextFieldConfig r) -> String -> Either String String
validateTextField config input = do
  let ValidationRule validate = config.validation
  if not (validate input) then Left config.errorMessage
  else if String.length input > config.maxLength then Left $ "Must be less than " <> show config.maxLength <> " characters"
  else Right $ config.formatInput input

validateNumberField :: forall r. Record (NumberFieldConfig r) -> String -> Either String Number
validateNumberField config input =
  case validateField input of
    Right value ->
      if value >= config.min && value <= config.max then Right value
      else Left $ "Must be between " <> show config.min <> " and " <> show config.max
    Left err -> Left err

validateFormField
  :: forall r a
   . FormValue a
  => FieldValidator a
  => Record (HTMLFormField r)
  -> ValidationResult a
validateFormField field =
  let
    validationResult = fromFormValue field.value
  in
    case validationResult of
      ValidationSuccess value ->
        if runValidation field.validation field.value then ValidationSuccess value
        else ValidationError (validationError (Proxy :: Proxy a))
      ValidationError err -> ValidationError err

validateField :: forall a. FormValue a => FieldValidator a => String -> Either String a
validateField str = do
  let trimmed = trim str
  if trimmed == "" then Left (validationError (Proxy :: Proxy a))
  else case fromFormValue trimmed of
    ValidationSuccess value -> Right value
    ValidationError err -> Left err

validateStringField :: String -> ValidationRule -> String -> Either String String
validateStringField fieldName (ValidationRule rule) value =
  if rule value
    then Right value
    else Left $ fieldName <> " validation failed"

validateMenuItem :: MenuItemFormInput -> Either String MenuItem
validateMenuItem input = do
  sort <- validateStringField "Sort" positiveInteger input.sort
  sku <- validateStringField "SKU" (allOf [nonEmpty, validUUID]) input.sku
  brand <- validateStringField "Brand" (allOf [nonEmpty, alphanumeric]) input.brand
  name <- validateStringField "Name" (allOf [nonEmpty, alphanumeric]) input.name
  price <- validateStringField "Price" dollarAmount input.price
  measure_unit <- validateStringField "Measure Unit" nonEmpty input.measure_unit
  per_package <- validateStringField "Per Package" nonEmpty input.per_package
  quantity <- validateStringField "Quantity" positiveInteger input.quantity
  category <- validateStringField "Category" nonEmpty input.category
  subcategory <- validateStringField "Subcategory" nonEmpty input.subcategory
  
  -- Validate strain lineage
  strainLineage <- validateStrainLineage input.strain_lineage

  -- Convert validated strings to final types
  sortNum <- case fromString sort of
    Just n -> Right n
    Nothing -> Left "Invalid sort number"
    
  skuUUID <- case parseUUID sku of
    Just uuid -> Right uuid
    Nothing -> Left "Invalid UUID format"
    
  priceNum <- case Number.fromString price of
    Just n -> Right n
    Nothing -> Left "Invalid price format"
    
  quantityNum <- case fromString quantity of
    Just n -> Right n
    Nothing -> Left "Invalid quantity format"
    
  categoryType <- validateCategory category
  
  pure $ MenuItem
    { sort: sortNum
    , sku: skuUUID
    , brand
    , name
    , price: priceNum
    , measure_unit
    , per_package
    , quantity: quantityNum
    , category: categoryType
    , subcategory
    , description: input.description
    , tags: parseCommaList input.tags
    , effects: parseCommaList input.effects
    , strain_lineage: strainLineage
    }

validateStrainLineage :: StrainLineageFormInput -> Either String StrainLineage
validateStrainLineage input = do
  thc <- validateStringField "THC" percentage input.thc
  cbg <- validateStringField "CBG" percentage input.cbg
  strain <- validateStringField "Strain" (allOf [nonEmpty, alphanumeric]) input.strain
  creator <- validateStringField "Creator" (allOf [nonEmpty, alphanumeric]) input.creator
  species <- validateStringField "Species" nonEmpty input.species
  dominant_tarpene <- validateStringField "Dominant Terpene" (allOf [nonEmpty, alphanumeric]) input.dominant_tarpene
  leafly_url <- validateStringField "Leafly URL" nonEmpty input.leafly_url
  img <- validateStringField "Image URL" nonEmpty input.img

  speciesType <- validateSpecies species

  pure $ StrainLineage
    { thc
    , cbg
    , strain
    , creator
    , species: speciesType
    , dominant_tarpene
    , tarpenes: parseCommaList input.tarpenes
    , lineage: parseCommaList input.lineage
    , leafly_url
    , img
    }

validateCategory :: String -> Either String ItemCategory
validateCategory = case _ of
  "Flower" -> Right Flower
  "PreRolls" -> Right PreRolls
  "Vaporizers" -> Right Vaporizers
  "Edibles" -> Right Edibles
  "Drinks" -> Right Drinks
  "Concentrates" -> Right Concentrates
  "Topicals" -> Right Topicals
  "Tinctures" -> Right Tinctures
  "Accessories" -> Right Accessories
  _ -> Left "Invalid category"

validateSpecies :: String -> Either String Species
validateSpecies = case _ of
  "Indica" -> Right Indica
  "IndicaDominantHybrid" -> Right IndicaDominantHybrid
  "Hybrid" -> Right Hybrid
  "SativaDominantHybrid" -> Right SativaDominantHybrid
  "Sativa" -> Right Sativa
  _ -> Left "Invalid species"