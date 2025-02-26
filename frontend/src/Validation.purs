module Validation where

import Prelude
import Types (ItemCategory(..), MenuItem(..), MenuItemFormInput, Species(..), StrainLineage(..), StrainLineageFormInput)

import Data.Array (any)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Number
import Data.String (joinWith)
import Data.String (length, toLower, trim) as String
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Validation.Semigroup (V, invalid, toEither, andThen)
import Types.UUID (UUID(..), parseUUID)
import Utils (parseCommaList)

type Validator a = String -> V (Array String) a

nonEmpty :: String -> Boolean
nonEmpty str = String.trim str /= ""

alphanumeric :: String -> Boolean
alphanumeric str = case regex "^[A-Za-z0-9-\\s]+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

extendedAlphanumeric :: String -> Boolean
extendedAlphanumeric str = case regex "^[A-Za-z0-9\\s\\-_&+',\\.\\(\\)]+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

percentage :: String -> Boolean
percentage str = case regex "^\\d{1,3}(\\.\\d{1,2})?%$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

dollarAmount :: String -> Boolean
dollarAmount str = case Number.fromString str of
  Just n -> n >= 0.0
  Nothing -> false

validMeasurementUnit :: String -> Boolean
validMeasurementUnit str =
  let
    units = [ "g", "mg", "kg", "oz", "lb", "ml", "l", "ea", "unit", "units", "pack", "packs", "eighth", "quarter", "half", "1/8", "1/4", "1/2" ]
    lowercaseStr = String.trim (str # String.toLower)
  in
    any (\unit -> unit == lowercaseStr) units

validUrl :: String -> Boolean
validUrl str =
  case regex "^(https?:\\/\\/)?(www\\.)?[a-zA-Z0-9][a-zA-Z0-9-]*(\\.[a-zA-Z0-9][a-zA-Z0-9-]*)+(\\/[\\w\\-\\.~:\\/?#[\\]@!$&'()*+,;=]*)*$" noFlags of
    Left _ -> false
    Right validRegex -> test validRegex str

positiveInteger :: String -> Boolean
positiveInteger str = case fromString str of
  Just n -> n > 0
  Nothing -> false

nonNegativeInteger :: String -> Boolean
nonNegativeInteger str = case fromString str of
  Just n -> n >= 0
  Nothing -> false

fraction :: String -> Boolean
fraction str = case regex "^\\d+\\/\\d+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

commaList :: String -> Boolean
commaList str = case regex "^[^,]+(,[^,]+)*$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

validUUID :: String -> Boolean
validUUID str = case parseUUID (String.trim str) of
  Just _ -> true
  Nothing -> false

maxLength :: Int -> String -> Boolean
maxLength n str = String.length str <= n

validateString :: String -> (String -> Boolean) -> Validator String
validateString errorMsg predicate = \str ->
  if predicate str
    then pure str
    else invalid [errorMsg]

validateAs :: forall a. String -> (String -> Maybe a) -> Validator a
validateAs errorMsg parser = \str ->
  case parser str of
    Just value -> pure value
    Nothing -> invalid [errorMsg]

required :: String -> Validator String
required errorMsg = validateString errorMsg nonEmpty

validateInt :: String -> Validator Int
validateInt errorMsg = \str ->
  case fromString str of
    Just n -> pure n
    Nothing -> invalid [errorMsg]

validateNumber :: String -> Validator Number
validateNumber errorMsg = \str ->
  case Number.fromString str of
    Just n -> pure n
    Nothing -> invalid [errorMsg]

validateUUID :: String -> Validator String
validateUUID errorMsg = validateString errorMsg validUUID

validateNonEmptyText :: Validator String
validateNonEmptyText = required "Field cannot be empty"

validateAlphanumeric :: Validator String
validateAlphanumeric = validateString "Must contain only letters, numbers, spaces, and hyphens" alphanumeric

validateExtendedText :: Int -> Validator String
validateExtendedText limit = \str ->
  if not (nonEmpty str)
    then invalid ["Field cannot be empty"]
  else if not (extendedAlphanumeric str)
    then invalid ["Must contain only allowed characters"]
  else if not (maxLength limit str)
    then invalid ["Must be less than " <> show limit <> " characters"]
  else pure str

validatePercentage :: Validator String
validatePercentage = validateString "Must be in format: XX.XX%" percentage

validateDollarAmount :: Validator Number
validateDollarAmount = \str ->
  case Number.fromString (String.trim str) of
    Just n | n >= 0.0 -> pure n
    _ -> invalid ["Must be a valid dollar amount"]

validateUnit :: Validator String
validateUnit = validateString "Must be a valid unit (g, mg, kg, oz, etc.)" validMeasurementUnit

validateUrl :: Validator String
validateUrl = validateString "Must be a valid URL" validUrl

validatePositiveInt :: Validator Int
validatePositiveInt = \str ->
  case fromString str of
    Just n | n > 0 -> pure n
    _ -> invalid ["Must be a positive whole number"]

validateNonNegativeInt :: Validator Int
validateNonNegativeInt = \str ->
  case fromString str of
    Just n | n >= 0 -> pure n
    _ -> invalid ["Must be a non-negative whole number"]

validateFraction :: Validator String
validateFraction = validateString "Must be a fraction (e.g. 1/2)" fraction

validateCommaList :: Validator String
validateCommaList = validateString "Must be a comma-separated list" commaList

withDefault :: forall a. a -> Validator a -> Validator a
withDefault defaultValue validator = \str ->
  if String.trim str == ""
    then pure defaultValue
    else validator str

optional :: forall a. Validator a -> Validator (Maybe a)
optional validator = \str ->
  if String.trim str == ""
    then pure Nothing
    else map Just (validator str)

validateForm :: forall a. V (Array String) a -> Either (Array String) a
validateForm = toEither

validateMenuItem :: MenuItemFormInput -> Either String MenuItem
validateMenuItem input = 
  case toEither validationResult of
    Left errors -> Left (joinErrors errors)
    Right result -> Right result
  where
    joinErrors :: Array String -> String
    joinErrors = joinWith ", "

    validationResult :: V (Array String) MenuItem
    validationResult =
      validateNonNegativeInt input.sort `andThen` \sort ->
      validateUUID "Required, must be a valid UUID" input.sku `andThen` \sku ->
      validateExtendedText 50 input.brand `andThen` \brand ->
      validateExtendedText 50 input.name `andThen` \name ->
      validateDollarAmount input.price `andThen` \price ->
      validateUnit input.measure_unit `andThen` \measure_unit ->
      validateString "Per package cannot be empty" nonEmpty input.per_package `andThen` \per_package ->
      validateNonNegativeInt input.quantity `andThen` \quantity ->
      validateCategory input.category `andThen` \category ->
      validateAlphanumeric input.subcategory `andThen` \subcategory ->
      validateStrainLineage' input.strain_lineage `andThen` \strainLineage ->

      pure $ MenuItem
        { sort: Int.fromString input.sort # fromMaybe 0
        , sku: parseUUID input.sku # fromMaybe (UUID "00000000-0000-0000-0000-000000000000")
        , brand
        , name
        , price: Number.fromString input.price # fromMaybe 0.0
        , measure_unit
        , per_package
        , quantity: Int.fromString input.quantity # fromMaybe 0
        , category
        , subcategory
        , description: input.description
        , tags: parseCommaList input.tags
        , effects: parseCommaList input.effects
        , strain_lineage: strainLineage
        }
    
    validateStrainLineage' = validateStrainLineage

validateStrainLineage :: StrainLineageFormInput -> V (Array String) StrainLineage
validateStrainLineage input =
  validatePercentage input.thc `andThen` \thc ->
  validatePercentage input.cbg `andThen` \cbg ->
  validateString "Strain cannot be empty" nonEmpty input.strain `andThen` \strain ->
  validateString "Creator cannot be empty" nonEmpty input.creator `andThen` \creator ->
  validateSpecies input.species `andThen` \species ->
  validateString "Dominant terpene cannot be empty" nonEmpty input.dominant_terpene `andThen` \dominant_terpene ->
  validateUrl input.leafly_url `andThen` \leafly_url ->
  validateUrl input.img `andThen` \img ->

  pure $ StrainLineage
    { thc
    , cbg
    , strain
    , creator
    , species
    , dominant_terpene
    , terpenes: parseCommaList input.terpenes
    , lineage: parseCommaList input.lineage
    , leafly_url
    , img
    }

validateCategory :: String -> V (Array String) ItemCategory
validateCategory input = case input of
  "Flower" -> pure Flower
  "PreRolls" -> pure PreRolls
  "Vaporizers" -> pure Vaporizers
  "Edibles" -> pure Edibles
  "Drinks" -> pure Drinks
  "Concentrates" -> pure Concentrates
  "Topicals" -> pure Topicals
  "Tinctures" -> pure Tinctures
  "Accessories" -> pure Accessories
  _ -> invalid ["Invalid category"]

validateSpecies :: String -> V (Array String) Species
validateSpecies input = case input of
  "Indica" -> pure Indica
  "IndicaDominantHybrid" -> pure IndicaDominantHybrid
  "Hybrid" -> pure Hybrid
  "SativaDominantHybrid" -> pure SativaDominantHybrid
  "Sativa" -> pure Sativa
  _ -> invalid ["Invalid species"]