module AddItem where

import Prelude

import BudView (InventoryResponse(..), ItemCategory(..), MenuItem(..), StrainLineage(..), postInventoryToJson, itemCategoryToString)
import Form
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

  -- Array fields
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
        -- , makeField tagsConfig setTags (const $ pure unit) (pure $ Just true)
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
            formInput <- liftEffect do
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
              pure $ 
                { name
                , sku
                , brand
                , price
                , quantity
                , category
                , description
                , tags: show tags
                , strainLineage:
                    { thc
                    , cbg
                    , strain
                    , creator
                    , species
                    , dominant_tarpene
                    , tarpenes: show tarpenes
                    , lineage: show lineage
                    , leafly_url: ""
                    , img: ""
                    }
                }
              
            case validateForm formInput of
              Right menuItem -> do
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
              Left validationError -> do
                liftEffect do
                  log $ "Validation error: " <> validationError
                  setStatusMessage $ "Validation error: " <> validationError
        ]
        [ text_ "Submit" ]
    , D.div
        [ DA.klass_ "mt-4 text-center" ]
        [ text statusMessageEvent ]
    ]