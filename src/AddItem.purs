module AddItem where

import Prelude

import BudView (InventoryResponse(..), ItemCategory(..), MenuItem(..), StrainLineage(..), postInventoryToJson)
import Data.Array (all, filter, (!!))
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
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut, Hook)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, useState')
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FRP.Poll (Poll, sample)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value) as Input
import Web.HTML.HTMLSelectElement (fromEventTarget, value) as Select
import Web.UIEvent.KeyboardEvent (toEvent)

-- Enhanced Validation Rules
type ValidationRule = String -> Boolean

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

-- Combine multiple validation rules
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

type MenuItemInput = 
  { name :: String
  , sku :: String
  , brand :: String
  , price :: String
  , quantity :: String
  , category :: String
  , description :: String
  , tags :: Array String
  , strainLineage :: 
      { thc :: String
      , cbg :: String
      , strain :: String
      , creator :: String
      , species :: String
      , dominant_tarpene :: String
      , tarpenes :: Array String
      , lineage :: Array String
      , leafly_url :: String
      , img :: String
      }
  }

createMenuItem :: MenuItemInput -> MenuItem
createMenuItem input = 
  MenuItem
    { sort: 0
    , sku: input.sku
    , brand: input.brand
    , name: input.name
    , price: fromMaybe 0.0 $ Num.fromString input.price
    , measure_unit: "units"
    , per_package: input.quantity
    , quantity: fromMaybe 0 $ fromString input.quantity
    , category: stringToItemCategory input.category
    , subcategory: input.category
    , description: input.description
    , tags: input.tags
    , strain_lineage: StrainLineage
        { thc: input.strainLineage.thc
        , cbg: input.strainLineage.cbg
        , strain: input.strainLineage.strain
        , creator: input.strainLineage.creator
        , species: input.strainLineage.species
        , dominant_tarpene: input.strainLineage.dominant_tarpene
        , tarpenes: input.strainLineage.tarpenes
        , lineage: input.strainLineage.lineage
        , leafly_url: input.strainLineage.leafly_url
        , img: input.strainLineage.img
        }
    }

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
       
app :: Effect Unit
app = void $ runInBody Deku.do
  setStatusMessage /\ statusMessageEvent <- useState ""
  
  -- Basic MenuItem fields
  setName /\ nameEvent <- useState'
  setValidName /\ validNameEvent <- useState (Nothing :: Maybe Boolean)

  setSku /\ skuEvent <- useState'
  setValidSku /\ validSkuEvent <- useState (Nothing :: Maybe Boolean)

  setBrand /\ brandEvent <- useState'
  setValidBrand /\ validBrandEvent <- useState (Nothing :: Maybe Boolean)

  setPrice /\ priceEvent <- useState'
  setValidPrice /\ validPriceEvent <- useState (Nothing :: Maybe Boolean)

  setQuantity /\ quantityEvent <- useState'
  setValidQuantity /\ validQuantityEvent <- useState (Nothing :: Maybe Boolean)

  setCategory /\ categoryEvent <- useState categoryConfig.defaultValue
  setValidCategory /\ validCategoryEvent <- useState (Nothing :: Maybe Boolean)

  setDescription /\ descriptionEvent <- useState'
  setValidDescription /\ validDescriptionEvent <- useState (Nothing :: Maybe Boolean)

  -- initialized as empty arrays
  setTags /\ tagsEvent <- useState ([] :: Array String)
  setTarpenes /\ tarpenesEvent <- useState ([] :: Array String)
  setLineage /\ lineageEvent <- useState ([] :: Array String)

  -- StrainLineage fields
  setThc /\ thcEvent <- useState'
  setValidThc /\ validThcEvent <- useState (Nothing :: Maybe Boolean)

  setCbg /\ cbgEvent <- useState'
  setValidCbg /\ validCbgEvent <- useState (Nothing :: Maybe Boolean)

  setStrain /\ strainEvent <- useState'
  setValidStrain /\ validStrainEvent <- useState (Nothing :: Maybe Boolean)

  setCreator /\ creatorEvent <- useState'
  setValidCreator /\ validCreatorEvent <- useState (Nothing :: Maybe Boolean)

  setSpecies /\ speciesEvent <- useState'
  setValidSpecies /\ validSpeciesEvent <- useState (Nothing :: Maybe Boolean)

  setDominantTarpene /\ dominantTarpeneEvent <- useState'
  setValidDominantTarpene /\ validDominantTarpeneEvent <- useState (Nothing :: Maybe Boolean)

  let  
    resetForm = do
      setName ""
      setValidName Nothing
      setSku ""
      setValidSku Nothing
      setBrand ""
      setValidBrand Nothing
      setPrice ""
      setValidPrice Nothing
      setQuantity ""
      setValidQuantity Nothing
      setCategory categoryConfig.defaultValue
      setValidCategory Nothing
      setDescription ""
      setValidDescription Nothing
      setTags []
      setThc ""
      setValidThc Nothing
      setCbg ""
      setValidCbg Nothing
      setStrain ""
      setValidStrain Nothing
      setCreator ""
      setValidCreator Nothing
      setSpecies ""
      setValidSpecies Nothing
      setDominantTarpene ""
      setValidDominantTarpene Nothing
      setTarpenes []
      setLineage []

    isFormValid = ado
      vName <- validNameEvent
      vSku <- validSkuEvent
      vBrand <- validBrandEvent
      vPrice <- validPriceEvent
      vQuantity <- validQuantityEvent
      vCategory <- validCategoryEvent
      vDescription <- validDescriptionEvent
      vThc <- validThcEvent
      vCbg <- validCbgEvent
      vStrain <- validStrainEvent
      vCreator <- validCreatorEvent
      vSpecies <- validSpeciesEvent
      vDominantTarpene <- validDominantTarpeneEvent
      in all (fromMaybe false) 
        [ vName, vSku, vBrand, vPrice, vQuantity, vCategory
        , vDescription, vThc, vCbg, vStrain, vCreator, vSpecies
        , vDominantTarpene ]

    isButtonDisabled = map (not >>> show) isFormValid

  D.div_
    [ D.div
        [ DA.klass_ "space-y-4 max-w-2xl mx-auto p-6" ]
        [ D.h2 
            [ DA.klass_ "text-2xl font-bold mb-6" ]
            [ text_ "Add New Menu Item" ]
        , makeField nameConfig setName setValidName validNameEvent
        , makeField skuConfig setSku setValidSku validSkuEvent 
        , makeField brandConfig setBrand setValidBrand validBrandEvent
        , makeField priceConfig setPrice setValidPrice validPriceEvent
        , makeField quantityConfig setQuantity setValidQuantity validQuantityEvent
        , makeDropdown categoryConfig setCategory setValidCategory validCategoryEvent
        , makeField descriptionConfig setDescription setValidDescription validDescriptionEvent
        , makeArrayField "Tags" setTags
        , makeField thcConfig setThc setValidThc validThcEvent
        , makeField cbgConfig setCbg setValidCbg validCbgEvent
        , makeField strainConfig setStrain setValidStrain validStrainEvent
        , makeField creatorConfig setCreator setValidCreator validCreatorEvent
        , makeField speciesConfig setSpecies setValidSpecies validSpeciesEvent
        , makeField dominantTarpeneConfig setDominantTarpene setValidDominantTarpene validDominantTarpeneEvent
        , makeArrayField "Terpenes" setTarpenes
        , makeArrayField "Lineage" setLineage
        ]
    , D.button
        [ DA.klass_ $ buttonClass "green"
        , DA.disabled isButtonDisabled
        , DL.click_ \_ -> void $ launchAff_ do
            values <- liftEffect do
              name <- sample nameEvent
              sku <- sample skuEvent
              brand <- sample brandEvent
              price <- sample priceEvent
              quantity <- sample quantityEvent
              category <- sample categoryEvent
              description <- sample descriptionEvent
              tags <- sample tagsEvent
              thc <- sample thcEvent
              cbg <- sample cbgEvent
              strain <- sample strainEvent
              creator <- sample creatorEvent
              species <- sample speciesEvent
              dominant_tarpene <- sample dominantTarpeneEvent
              tarpenes <- sample tarpenesEvent
              lineage <- sample lineageEvent 
              pure { name
                  , sku
                  , brand
                  , price
                  , quantity
                  , category
                  , description
                  , tags
                  , strainLineage:
                      { thc
                      , cbg
                      , strain
                      , creator
                      , species
                      , dominant_tarpene
                      , tarpenes
                      , lineage
                      , leafly_url: ""
                      , img: ""
                      }
                  }
              
            let menuItem = createMenuItem
                  { name: values.name
                  , sku: values.sku
                  , brand: values.brand
                  , price: values.price
                  , quantity: values.quantity
                  , category: values.category
                  , description: values.description
                  , tags: values.tags
                  , strainLineage:
                      { thc: values.thc
                      , cbg: values.cbg
                      , strain: values.strain
                      , creator: values.creator
                      , species: values.species
                      , dominant_tarpene: values.dominant_tarpene
                      , tarpenes: values.tarpenes
                      , lineage: values.lineage
                      , leafly_url: ""
                      , img: ""
                      }
                  }
            
            result <- postInventoryToJson menuItem
            liftEffect $ case result of
              Right (Message msg) -> do
                log $ "Success: " <> msg
                setStatusMessage "Item successfully submitted!"
                resetForm
              Right (InventoryData _) -> do
                log "Received inventory data instead of confirmation message"
                setStatusMessage "Unexpected response type"
              Left err -> do
                log $ "Error: " <> err
                setStatusMessage $ "Error submitting item: " <> err
        ]
        [ text_ "Submit" ]
    , D.div
        [ DA.klass_ "mt-4 text-center" ]
        [ text statusMessageEvent ]
    , D.div_
        [ text $ map (\val -> "Name: " <> val) nameEvent
        , D.br_ []
        , text $ map (\val -> "SKU: " <> val) skuEvent
        , D.br_ []
        , text $ map (\val -> "Brand: " <> val) brandEvent
        , D.br_ []
        , text $ map (\val -> "Price: $" <> val) priceEvent
        , D.br_ []
        , text $ map (\val -> "Quantity: " <> val) quantityEvent
        , D.br_ []
        , text $ map (\val -> "Category: " <> val) categoryEvent
        , D.br_ []
        , text $ map (\val -> "Description: " <> val) descriptionEvent
        , D.br_ []
        , text $ map (\arr -> "Tags: " <> show arr) tagsEvent
        , D.br_ []
        , text $ map (\val -> "THC: " <> val) thcEvent
        , D.br_ []
        , text $ map (\val -> "CBG: " <> val) cbgEvent
        , D.br_ []
        , text $ map (\val -> "Strain: " <> val) strainEvent
        , D.br_ []
        , text $ map (\val -> "Creator: " <> val) creatorEvent
        , D.br_ []
        , text $ map (\val -> "Species: " <> val) speciesEvent
        , D.br_ []
        , text $ map (\val -> "Dominant Terpene: " <> val) dominantTarpeneEvent
        , D.br_ []
        , text $ map (\arr -> "Terpenes: " <> show arr) tarpenesEvent
        , D.br_ []
        , text $ map (\arr -> "Lineage: " <> show arr) lineageEvent
        ]
    ]