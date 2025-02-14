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

validateMenuItem :: MenuItemFormInput -> Either String MenuItem
validateMenuItem input = do
  sku <- validateField input.sku :: Either String UUID
  name <- validateField input.name :: Either String String
  brand <- validateField input.brand :: Either String String
  price <- validateField input.price :: Either String Number
  quantity <- validateField input.quantity :: Either String Int
  category <- validateField input.category :: Either String ItemCategory

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
    , description: input.description
    , tags: parseCommaList input.tags
    , effects: parseCommaList input.effects
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