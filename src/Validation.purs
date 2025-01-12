module Validation where
  
import Prelude

import Data.Array (all)
import Data.Either (Either(..), note)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Number
import Data.String (length) as String
import Data.String (trim)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Types (class FormValue, FieldConfigRow, FieldValidator, HTMLFormField, ItemCategory, MenuItem(..), MenuItemFormInput, Species, StrainLineage(..), StrainLineageFormInput, TextFieldConfig, ValidationPreset, ValidationResult(..), ValidationRule, NumberFieldConfig, fromFormValue)
import UUID (UUID, parseUUID)
import Utils (formatDollarAmount, parseCommaList)

-- | Basic validation rules
requireValid :: ∀ a. String -> ValidationResult a -> Either String a
requireValid field = case _ of
  ValidationSuccess x -> Right x
  ValidationError err -> Left $ field <> ": " <> err

nonEmpty :: ValidationRule
nonEmpty = (_ /= "")

validUUID :: ValidationRule
validUUID str = case parseUUID (trim str) of
  Just _ -> true
  Nothing -> false

alphanumeric :: ValidationRule
alphanumeric str = case regex "^[A-Za-z0-9-\\s]+$" noFlags of
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

positiveInteger :: ValidationRule
positiveInteger str = case fromString str of
  Just n -> n > 0
  Nothing -> false

vowels :: ValidationRule
vowels str = case regex "^[AEIOUYaeiouy\\s]+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

consonants :: ValidationRule
consonants str = case regex "^[BCDFGHJKLMNPQRSTVWXZbcdfghjklmnpqrstvwxz\\s]+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

commaList :: ValidationRule
commaList str = 
  case regex "^[^,]+(,[^,]+)*$" noFlags of
    Left _ -> false
    Right validRegex -> test validRegex str

maxLength :: Int -> ValidationRule
maxLength n str = String.length str <= n

allOf :: Array ValidationRule -> ValidationRule
allOf rules str = all (\rule -> rule str) rules

-- | Validation presets
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
validateForm config input = do
  -- Basic field validations
  name <- validateTextField 
    { label: "Name"
    , maxLength: 50
    , placeholder: "Enter name"
    , defaultValue: ""
    , validation: allOf [nonEmpty, alphanumeric]
    , errorMessage: "Required, text only (max 50 chars)"
    , formatInput: trim
    } input.name

  sku <- validateField uuidValidator input.sku

  brand <- validateTextField
    { label: "Brand"
    , maxLength: 30
    , placeholder: "Enter brand"
    , defaultValue: ""
    , validation: allOf [nonEmpty, alphanumeric]
    , errorMessage: "Required, text only (max 30 chars)"
    , formatInput: trim
    } input.brand

  price <- validateNumberField
    { label: "Price"
    , min: 0.0
    , max: 999999.99
    , placeholder: "Enter price"
    , defaultValue: ""
    , validation: allOf [nonEmpty, dollarAmount]
    , errorMessage: "Required, valid dollar amount"
    , formatInput: formatDollarAmount
    } input.price

  quantity <- validateNumberField
    { label: "Quantity"
    , min: 0.0
    , max: 999999.0
    , placeholder: "Enter quantity"
    , defaultValue: ""
    , validation: allOf [nonEmpty, positiveInteger]
    , errorMessage: "Required, positive whole number"
    , formatInput: \str -> fromMaybe str $ map show $ fromString str
    } input.quantity

  category <- validateField categoryValidator input.category

  description <- validateTextField
    { label: "Description"
    , maxLength: 1000
    , placeholder: "Enter description"
    , defaultValue: ""
    , validation: nonEmpty
    , errorMessage: "Required"
    , formatInput: identity
    } input.description

  -- Strain lineage validation
  strainLineage <- validateStrainLineage 
    { thc: input.strainLineage.thc
    , cbg: input.strainLineage.cbg
    , strain: input.strainLineage.strain
    , creator: input.strainLineage.creator
    , species: input.strainLineage.species
    , dominant_tarpene: input.strainLineage.dominant_tarpene
    , tarpenes: input.strainLineage.tarpenes
    , lineage: input.strainLineage.lineage
    }

  -- Construct the final MenuItem with all validated fields
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
    , effects: parseCommaList input.effects
    , strain_lineage: strainLineage
    }

validateTextField :: forall r. Record (TextFieldConfig r) -> String -> Either String String
validateTextField config input =
  case validateField (requiredField Just config.label) input of
    Right value -> 
      if String.length value <= config.maxLength
      then Right value
      else Left $ "Must be less than " <> show config.maxLength <> " characters"
    Left err -> Left err

validateNumberField :: forall r. Record (NumberFieldConfig r) -> String -> Either String Number
validateNumberField config input =
  case validateField numberValidator input of
    Right value ->
      if value >= config.min && value <= config.max
      then Right value
      else Left $ "Must be between " <> show config.min <> " and " <> show config.max
    Left err -> Left err

validateFormField :: forall r a
   . FormValue a
  => Record (HTMLFormField r)
  -> ValidationResult a
validateFormField field = do
  let validationResult = fromFormValue field.value
  case validationResult of
    ValidationSuccess value -> 
      if field.validation field.value
      then ValidationSuccess value
      else ValidationError err
    ValidationError err -> ValidationError err

validateField :: forall a. FieldValidator a -> String -> Either String a
validateField validator input = do
  let trimmed = trim input
  _ <- note validator.error $ 
    if validator.validate trimmed 
    then Just trimmed
    else Nothing
  note validator.error $ validator.convert trimmed

-- validateTextField :: forall r. Record (TextFieldConfig r) -> String -> ValidationResult String
-- validateTextField config input =
--   let baseValidation = validateField (requiredField Just config.label) input
--   in case baseValidation of
--     Right value -> 
--       if String.length value <= config.maxLength
--       then ValidationSuccess value
--       else ValidationError $ "Must be less than " <> show config.maxLength <> " characters"
--     Left err -> ValidationError err

validateMenuItem :: MenuItemFormInput -> Either String MenuItem
validateMenuItem input = do
  sku <- validateField uuidValidator input.sku
  name <- validateField (requiredField Just "Name") input.name
  brand <- validateField (requiredField Just "Brand") input.brand
  price <- validateField numberValidator input.price
  quantity <- validateField intValidator input.quantity
  category <- validateField categoryValidator input.category
  
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
  thc <- validateField (requiredField Just "THC") input.thc
  cbg <- validateField (requiredField Just "CBG") input.cbg
  strain <- validateField (requiredField Just "Strain") input.strain
  creator <- validateField (requiredField Just "Creator") input.creator
  species <- validateField speciesValidator input.species
  dominant_tarpene <- validateField (requiredField Just "Dominant Terpene") input.dominant_tarpene

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

requiredField :: forall a. (String -> Maybe a) -> String -> FieldValidator a
requiredField convert field = 
  { validate: (_ /= "")
  , convert
  , error: field <> " is required"
  }

-- | Specific validators for each field type
numberValidator :: FieldValidator Number
numberValidator = 
  { validate: \s -> case Number.fromString s of
      Just n -> n >= 0.0
      Nothing -> false
  , convert: Number.fromString
  , error: "Must be a valid number"
  }

intValidator :: FieldValidator Int
intValidator = requiredField fromString "Integer"

uuidValidator :: FieldValidator UUID
uuidValidator = 
  { validate: \s -> case parseUUID (trim s) of
      Just _ -> true
      Nothing -> false
  , convert: parseUUID
  , error: "Must be a valid UUID"
  }

categoryValidator :: FieldValidator ItemCategory
categoryValidator = 
  { validate: \s -> case (fromFormValue s :: ValidationResult ItemCategory) of
      ValidationSuccess _ -> true
      _ -> false
  , convert: \s -> case (fromFormValue s :: ValidationResult ItemCategory) of
      ValidationSuccess cat -> Just cat
      _ -> Nothing
  , error: "Must be a valid category"
  }

speciesValidator :: FieldValidator Species
speciesValidator = 
  { validate: \s -> case (fromFormValue s :: ValidationResult Species) of
      ValidationSuccess _ -> true
      _ -> false
  , convert: \s -> case (fromFormValue s :: ValidationResult Species) of
      ValidationSuccess species -> Just species
      _ -> Nothing
  , error: "Must be a valid species"
  }