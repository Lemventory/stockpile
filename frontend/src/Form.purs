module Form where

import Prelude

import Data.Array (null, (:))
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum)
import Data.Foldable (for_)
import Data.String (Pattern(..), Replacement(..), trim, replaceAll)
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (toEither)
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Effect (Effect)
import Types (DropdownConfig, ItemCategory, Species)
import Utils (getAllEnumValues, parseCommaList)
import Validation as V
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value) as Input
import Web.HTML.HTMLSelectElement (fromEventTarget, value) as Select
import Web.UIEvent.KeyboardEvent (toEvent)

-- | Modern field types

-- Configuration for a field with validations
type FieldConfig a = {
  label :: String,
  placeholder :: String,
  defaultValue :: String,
  validator :: V.Validator a,
  formatter :: String -> String
}

-- | Form field components

makeField :: forall a. FieldConfig a -> (String -> Effect Unit) -> (a -> Effect Unit) -> (Array String -> Effect Unit) -> Nut
makeField config setValue setValidValue setErrors = Deku.do
  setFieldErrors /\ fieldErrors <- useState []
  
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
                for_ (target targetEvent >>= Input.fromEventTarget) \inputElement -> do
                  input <- Input.value inputElement
                  let formatted = config.formatter input
                  setValue formatted
                  
                  -- Validate using the modern approach
                  let result = config.validator formatted
                  case toEither result of
                    Right validValue -> do
                      setValidValue validValue
                      setFieldErrors []
                      setErrors []
                    Left errs -> do
                      setFieldErrors errs
                      setErrors errs
            , DA.klass_ inputKls
            ]
            []
        , D.div
            [ DA.klass_ "error-container" ]
            [ fieldErrors <#~> \errs ->
                if null errs then
                  D.span [] []
                else
                  D.ul [ DA.klass_ "text-red-500 text-xs" ] 
                    (map (\err -> D.li_ [ text_ err ]) errs)
            ]
        ]
    ]

makeTextarea :: forall a. FieldConfig a -> (String -> Effect Unit) -> (a -> Effect Unit) -> (Array String -> Effect Unit) -> Nut
makeTextarea config setValue setValidValue setErrors = Deku.do
  setFieldErrors /\ fieldErrors <- useState []
  
  D.div_
    [ D.div
        [ DA.klass_ "flex items-center gap-2" ]
        [ D.label_
            [ text_ config.label ]
        , D.textarea
            [ DA.placeholder_ config.placeholder
            , DA.cols_ "40"
            , DA.rows_ "4"
            , DL.keyup_ \evt -> do
                let targetEvent = toEvent evt
                for_ (target targetEvent >>= Input.fromEventTarget) \inputElement -> do
                  input <- Input.value inputElement
                  let formatted = config.formatter input
                  setValue formatted
                  
                  -- Validate using the modern approach
                  let result = config.validator formatted
                  case toEither result of
                    Right validValue -> do
                      setValidValue validValue
                      setFieldErrors []
                      setErrors []
                    Left errs -> do
                      setFieldErrors errs
                      setErrors errs
            , DA.klass_ (inputKls <> " resize-y")
            ]
            [ text_ config.defaultValue ]
        , D.div
            [ DA.klass_ "error-container" ]
            [ fieldErrors <#~> \errs ->
                if null errs then
                  D.span [] []
                else
                  D.ul [ DA.klass_ "text-red-500 text-xs" ] 
                    (map (\err -> D.li_ [ text_ err ]) errs)
            ]
        ]
    ]

makeDropdown :: forall a. DropdownConfig -> (String -> Effect Unit) -> (a -> Effect Unit) -> (Array String -> Effect Unit) -> V.Validator a -> Nut
makeDropdown config setValue setValidValue setErrors validator = Deku.do
  setFieldErrors /\ fieldErrors <- useState []
  
  D.div_
    [ D.div
        [ DA.klass_ "flex items-center gap-2" ]
        [ D.label_
            [ text_ config.label ]
        , D.select
            [ DA.klass_ inputKls
            , DL.change_ \evt -> do
                for_ (target evt >>= Select.fromEventTarget) \selectElement -> do
                  value <- Select.value selectElement
                  setValue value
                  
                  let result = validator value
                  case toEither result of
                    Right validValue -> do
                      setValidValue validValue
                      setFieldErrors []
                      setErrors []
                    Left errs -> do
                      setFieldErrors errs
                      setErrors errs
            ]
            ( config.options <#> \opt ->
                D.option
                  [ DA.value_ opt.value ]
                  [ text_ opt.label ]
            )
        , D.div
            [ DA.klass_ "error-container" ]
            [ fieldErrors <#~> \errs ->
                if null errs then
                  D.span [] []
                else
                  D.ul [ DA.klass_ "text-red-500 text-xs" ] 
                    (map (\err -> D.li_ [ text_ err ]) errs)
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
                for_ ((target >=> Input.fromEventTarget) (toEvent evt)) \inputElement -> do
                  v <- Input.value inputElement
                  setValue $ parseCommaList v
            , DA.klass_ inputKls
            ]
            []
        ]
    ]

-- | Field configurations

-- Name field
nameFieldConfig :: FieldConfig String
nameFieldConfig = {
  label: "Name",
  placeholder: "Enter product name",
  defaultValue: "",
  validator: V.validateExtendedText 50,
  formatter: trim
}

-- SKU field
skuFieldConfig :: String -> FieldConfig String
skuFieldConfig defaultValue = {
  label: "SKU",
  placeholder: "Enter UUID",
  defaultValue,
  validator: V.validateUUID "Required, must be a valid UUID",
  formatter: trim
}

-- Brand field
brandFieldConfig :: String -> FieldConfig String
brandFieldConfig defaultValue = {
  label: "Brand",
  placeholder: "Enter brand name",
  defaultValue,
  validator: V.validateExtendedText 50,
  formatter: trim
}

-- Price field
priceFieldConfig :: String -> FieldConfig Number
priceFieldConfig defaultValue = {
  label: "Price",
  placeholder: "Enter price",
  defaultValue,
  validator: V.validateDollarAmount,
  formatter: trim
}

-- Quantity field
quantityFieldConfig :: String -> FieldConfig Int
quantityFieldConfig defaultValue = {
  label: "Quantity",
  placeholder: "Enter quantity",
  defaultValue,
  validator: V.validateNonNegativeInt,
  formatter: trim
}

-- THC field
thcFieldConfig :: String -> FieldConfig String
thcFieldConfig defaultValue = {
  label: "THC %",
  placeholder: "Enter THC percentage",
  defaultValue,
  validator: V.validatePercentage,
  formatter: trim
}

-- CBG field
cbgFieldConfig :: String -> FieldConfig String
cbgFieldConfig defaultValue = {
  label: "CBG %",
  placeholder: "Enter CBG percentage",
  defaultValue,
  validator: V.validatePercentage,
  formatter: trim
}

-- Strain field
strainFieldConfig :: String -> FieldConfig String
strainFieldConfig defaultValue = {
  label: "Strain",
  placeholder: "Enter strain name",
  defaultValue,
  validator: V.validateNonEmptyText,
  formatter: trim
}

-- Creator field
creatorFieldConfig :: String -> FieldConfig String
creatorFieldConfig defaultValue = {
  label: "Creator",
  placeholder: "Enter creator name",
  defaultValue,
  validator: V.validateNonEmptyText,
  formatter: trim
}

-- Dominant Terpene field
dominantTerpeneFieldConfig :: String -> FieldConfig String
dominantTerpeneFieldConfig defaultValue = {
  label: "Dominant Terpene",
  placeholder: "Enter dominant terpene",
  defaultValue,
  validator: V.validateNonEmptyText,
  formatter: trim
}

-- Description field
descriptionFieldConfig :: String -> FieldConfig String
descriptionFieldConfig defaultValue = {
  label: "Description",
  placeholder: "Enter description",
  defaultValue,
  validator: V.validateNonEmptyText, 
  formatter: identity
}

-- Tags field
tagsFieldConfig :: String -> FieldConfig String
tagsFieldConfig defaultValue = {
  label: "Tags",
  placeholder: "Enter tags (comma-separated)",
  defaultValue,
  validator: V.validateCommaList,
  formatter: trim
}

-- Effects field
effectsFieldConfig :: String -> FieldConfig String
effectsFieldConfig defaultValue = {
  label: "Effects",
  placeholder: "Enter effects (comma-separated)",
  defaultValue,
  validator: V.validateCommaList,
  formatter: trim
}

-- Terpenes field
terpenesFieldConfig :: String -> FieldConfig String
terpenesFieldConfig defaultValue = {
  label: "Terpenes",
  placeholder: "Enter terpenes (comma-separated)",
  defaultValue,
  validator: V.validateCommaList,
  formatter: trim
}

-- Lineage field
lineageFieldConfig :: String -> FieldConfig String
lineageFieldConfig defaultValue = {
  label: "Lineage",
  placeholder: "Enter lineage (comma-separated)",
  defaultValue,
  validator: V.validateCommaList,
  formatter: trim
}

-- Sort field
sortFieldConfig :: String -> FieldConfig Int
sortFieldConfig defaultValue = {
  label: "Sort Order",
  placeholder: "Enter sort position",
  defaultValue,
  validator: V.validateNonNegativeInt,
  formatter: trim
}

-- Measure Unit field
measureUnitFieldConfig :: String -> FieldConfig String
measureUnitFieldConfig defaultValue = {
  label: "Measure Unit",
  placeholder: "Enter unit (g, mg, etc)",
  defaultValue,
  validator: V.validateUnit,
  formatter: trim
}

-- Per Package field
perPackageFieldConfig :: String -> FieldConfig String
perPackageFieldConfig defaultValue = {
  label: "Per Package",
  placeholder: "Enter amount per package",
  defaultValue,
  validator: V.validateString "Required, whole number or fraction" 
    (\str -> V.nonNegativeInteger str || V.fraction str),
  formatter: identity
}

-- Subcategory field
subcategoryFieldConfig :: String -> FieldConfig String
subcategoryFieldConfig defaultValue = {
  label: "Subcategory",
  placeholder: "Enter subcategory",
  defaultValue,
  validator: V.validateAlphanumeric,
  formatter: trim
}

-- Leafly URL field
leaflyUrlFieldConfig :: String -> FieldConfig String
leaflyUrlFieldConfig defaultValue = {
  label: "Leafly URL",
  placeholder: "Enter Leafly URL",
  defaultValue,
  validator: V.validateUrl,
  formatter: trim
}

-- Image URL field
imgFieldConfig :: String -> FieldConfig String
imgFieldConfig defaultValue = {
  label: "Image URL",
  placeholder: "Enter image URL",
  defaultValue,
  validator: V.validateUrl,
  formatter: trim
}

-- | Dropdown configurations

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