module Fields where

import Prelude

import Data.String (trim)
import Form (makeEnumDropdown)
import Validation (allOf, commaListField, moneyField, multilineText, nonEmpty, numberField, percentageField, requiredText, requiredTextWithLimit, validUUID)
import Types (DropdownConfig, FieldConfig, ItemCategory, Species, ValidationPreset)

-- | Field configurations
makeFieldConfig :: String -> String -> String -> ValidationPreset -> FieldConfig
makeFieldConfig label placeholder defaultValue preset =
  { label
  , placeholder
  , defaultValue
  , validation: preset.validation
  , errorMessage: preset.errorMessage
  , formatInput: preset.formatInput
  }

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

dominantTarpeneConfig :: String -> FieldConfig
dominantTarpeneConfig defaultValue = makeFieldConfig "Dominant Terpene" "Enter dominant terpene" defaultValue 
  requiredText

descriptionConfig :: String -> FieldConfig
descriptionConfig defaultValue = makeFieldConfig "Description" "Enter description" defaultValue multilineText

tagsConfig :: String -> FieldConfig
tagsConfig defaultValue = makeFieldConfig "Tags" "Enter tags (comma-separated)" defaultValue commaListField

effectsConfig :: String -> FieldConfig
effectsConfig defaultValue = makeFieldConfig "Effects" "Enter effects (comma-separated)" defaultValue commaListField

tarpenesConfig :: String -> FieldConfig
tarpenesConfig defaultValue = makeFieldConfig "Terpenes" "Enter terpenes (comma-separated)" defaultValue commaListField

lineageConfig :: String -> FieldConfig
lineageConfig defaultValue = makeFieldConfig "Lineage" "Enter lineage (comma-separated)" defaultValue commaListField