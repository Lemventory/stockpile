module Form where

import Prelude

import Data.Array ((:))
import Data.Enum (class BoundedEnum)
import Data.Foldable (for_)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), trim, replaceAll)
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Effect (Effect)
import FRP.Poll (Poll)
import Types (DropdownConfig, FieldConfig, ItemCategory, Species, ValidationPreset, runValidation)
import Utils (getAllEnumValues, parseCommaList)
import Validation (allOf, alphanumeric, anyOf, commaListField, fraction, moneyField, multilineText, nonEmpty, nonNegativeInteger, percentageField, quantityField, requiredText, requiredTextWithLimit, urlField, validMeasurementUnit, validUUID)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value) as Input
import Web.HTML.HTMLSelectElement (fromEventTarget, value) as Select
import Web.UIEvent.KeyboardEvent (toEvent)

makeField :: FieldConfig -> (String -> Effect Unit) -> (Maybe Boolean -> Effect Unit) -> Poll (Maybe Boolean) -> Nut
makeField config setValue setValid validEvent =
  D.div_
    [ D.div
        [ DA.klass_ "flex items-center gap-2" ]
        [ D.label_
            [ text_ config.label ]
        , if config.label == "Description" then D.textarea
            [ DA.placeholder_ config.placeholder
            , DA.cols_ "40"
            , DA.rows_ "4"
            , DL.keyup_ \evt -> do
                let targetEvent = toEvent evt
                for_
                  (target targetEvent >>= Input.fromEventTarget)
                  \inputElement -> do
                    v <- Input.value inputElement
                    let formatted = config.formatInput v
                    setValue formatted
                    setValid (Just (runValidation config.validation formatted))
            , DL.input_ \evt -> do
                for_
                  (target evt >>= Input.fromEventTarget)
                  \inputElement -> do
                    v <- Input.value inputElement
                    let formatted = config.formatInput v
                    setValue formatted
                    setValid (Just (runValidation config.validation formatted))
            , DA.klass_ (inputKls <> " resize-y")
            ]
            [ text_ config.defaultValue ]
          else D.input
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
                    setValid (Just (runValidation config.validation formatted))
            , DL.input_ \evt -> do
                for_
                  (target evt >>= Input.fromEventTarget)
                  \inputElement -> do
                    v <- Input.value inputElement
                    let formatted = config.formatInput v
                    setValue formatted
                    setValid (Just (runValidation config.validation formatted))
            , DA.klass_ inputKls
            ]
            []
        , D.span
            [ DA.klass_ "text-red-500 text-xs" ]
            [ text
                ( map
                    ( \mValid -> case mValid of
                        Just false -> config.errorMessage
                        _ -> ""
                    )
                    validEvent
                )
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
            ( config.options <#> \opt ->
                D.option
                  [ DA.value_ opt.value ]
                  [ text_ opt.label ]
            )
        , D.span
            [ DA.klass_ "text-red-500 text-xs" ]
            [ text
                ( map
                    ( \mValid -> case mValid of
                        Just false -> "Please select an option"
                        _ -> ""
                    )
                    validEvent
                )
            ]
        ]
    ]

makeEnumDropdown
  :: ∀ a
   . BoundedEnum a
  => Bounded a
  => Show a
  => { label :: String, enumType :: a }
  -> DropdownConfig
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
  { validation: allOf [ nonEmpty, validUUID ]
  , errorMessage: "Required, must be a valid UUID"
  , formatInput: trim
  }

nameConfig :: String -> FieldConfig
nameConfig defaultValue = makeFieldConfig "Name" "Enter product name" defaultValue
  (requiredTextWithLimit 50)

brandConfig :: String -> FieldConfig
brandConfig defaultValue = makeFieldConfig "Brand" "Enter brand name" defaultValue
  (requiredTextWithLimit 50)

priceConfig :: String -> FieldConfig
priceConfig defaultValue = makeFieldConfig "Price" "Enter price" defaultValue
  moneyField

quantityConfig :: String -> FieldConfig
quantityConfig defaultValue = makeFieldConfig "Quantity" "Enter quantity" defaultValue
  quantityField

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

sortConfig :: String -> FieldConfig
sortConfig defaultValue = makeFieldConfig "Sort Order" "Enter sort position" defaultValue
  { validation: allOf [ nonEmpty, nonNegativeInteger ]
  , errorMessage: "Required, non-negative whole number"
  , formatInput: \str -> fromMaybe str $ map show $ fromString str
  }
  
measureUnitConfig :: String -> FieldConfig
measureUnitConfig defaultValue = makeFieldConfig "Measure Unit" "Enter unit (g, mg, etc)" defaultValue
  { validation: validMeasurementUnit
  , errorMessage: "Required, valid unit (g, mg, kg, oz, etc.)"
  , formatInput: trim
  }

perPackageConfig :: String -> FieldConfig
perPackageConfig defaultValue = makeFieldConfig "Per Package" "Enter amount per package" defaultValue
  { validation: anyOf [ nonNegativeInteger, fraction ]
  , errorMessage: "Required, whole number or fraction"
  , formatInput: identity
  }

subcategoryConfig :: String -> FieldConfig
subcategoryConfig defaultValue = makeFieldConfig "Subcategory" "Enter subcategory" defaultValue
  { validation: allOf [ nonEmpty, alphanumeric ]
  , errorMessage: "Required, text only"
  , formatInput: trim
  }

leaflyUrlConfig :: String -> FieldConfig
leaflyUrlConfig defaultValue = makeFieldConfig "Leafly URL" "Enter Leafly URL" defaultValue urlField

imgConfig :: String -> FieldConfig
imgConfig defaultValue = makeFieldConfig "Image URL" "Enter image URL" defaultValue urlField

-- | Styling
inputKls :: String
inputKls =
  """
  rounded-md border-gray-300 shadow-sm
  border-2 mr-2 border-solid
  focus:border-indigo-500 focus:ring-indigo-500
  sm:text-sm
"""

buttonClass :: String -> String
buttonClass color =
  replaceAll (Pattern "COLOR") (Replacement color)
    """
    mb-3 inline-flex items-center rounded-md
    border border-transparent bg-COLOR-600 px-3 py-2
    text-sm font-medium leading-4 text-white shadow-sm
    hover:bg-COLOR-700 focus:outline-none focus:ring-2
    focus:ring-COLOR-500 focus:ring-offset-2
"""