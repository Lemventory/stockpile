module Validation where
  
import Prelude
import Utils (formatDollarAmount, parseCommaList)
import UUID (UUID, parseUUID)
import Types (FieldValidator, ItemCategory, MenuItem(..), MenuItemFormInput, Species, StrainLineage(..), StrainLineageFormInput, ValidationPreset, ValidationResult(..), ValidationRule, fromFormValue)

import Data.Array (all)
import Data.Either (Either(..), note)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Number
import Data.String (trim)
import Data.String (length) as String
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)

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
    , effects: parseCommaList input.effects
    , strain_lineage: strainLineage
    }

validateField :: forall a. FieldValidator a -> String -> Either String a
validateField validator input = do
  let trimmed = trim input
  _ <- note validator.error $ 
    if validator.validate trimmed 
    then Just trimmed
    else Nothing
  note validator.error $ validator.convert trimmed

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