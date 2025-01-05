module CreateItem where

import Prelude

import Types (InventoryResponse(..), ItemCategory(..), itemCategoryToString)
import API (postInventoryToJson)
import Form (MenuItemFormInput, brandConfig, buttonClass, categoryConfig, cbgConfig, creatorConfig, descriptionConfig, dominantTarpeneConfig, lineageConfig, makeDropdown, makeField, nameConfig, priceConfig, quantityConfig, skuConfig, speciesConfig, strainConfig, tagsConfig, tarpenesConfig, thcConfig, validateForm)
import Control.Monad.ST.Class (liftST)
import Data.Array (all)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Num
import Data.String (Pattern(..), trim)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create, subscribe)
import FRP.Poll (sample_)

createItem :: Effect Unit
createItem = void $ runInBody Deku.do
  -- Status and loading state
  setStatusMessage /\ statusMessageEvent <- useState ""
  setSubmitting /\ submittingEvent <- useState false
  setFiber /\ fiber <- useState (pure unit)

  -- Basic MenuItem fields
  setName /\ nameEvent <- useState ""
  setValidName /\ validNameEvent <- useState (Nothing :: Maybe Boolean)

  setSku /\ skuEvent <- useState ""
  setValidSku /\ validSkuEvent <- useState (Nothing :: Maybe Boolean)

  setBrand /\ brandEvent <- useState ""
  setValidBrand /\ validBrandEvent <- useState (Nothing :: Maybe Boolean)

  setPrice /\ priceEvent <- useState ""  
  setValidPrice /\ validPriceEvent <- useState (Nothing :: Maybe Boolean)

  setQuantity /\ quantityEvent <- useState ""
  setValidQuantity /\ validQuantityEvent <- useState (Nothing :: Maybe Boolean)

  setCategory /\ categoryEvent <- useState categoryConfig.defaultValue
  setValidCategory /\ validCategoryEvent <- useState (Nothing :: Maybe Boolean)

  setDescription /\ descriptionEvent <- useState ""
  setValidDescription /\ validDescriptionEvent <- useState (Nothing :: Maybe Boolean)

  -- Array fields (stored as comma-separated strings)
  setTags /\ tagsEvent <- useState ""
  setTarpenes /\ tarpenesEvent <- useState ""
  setLineage /\ lineageEvent <- useState ""

  -- StrainLineage fields
  setThc /\ thcEvent <- useState ""
  setValidThc /\ validThcEvent <- useState (Nothing :: Maybe Boolean)

  setCbg /\ cbgEvent <- useState ""
  setValidCbg /\ validCbgEvent <- useState (Nothing :: Maybe Boolean)

  setStrain /\ strainEvent <- useState ""
  setValidStrain /\ validStrainEvent <- useState (Nothing :: Maybe Boolean)

  setCreator /\ creatorEvent <- useState ""
  setValidCreator /\ validCreatorEvent <- useState (Nothing :: Maybe Boolean)

  setSpecies /\ speciesEvent <- useState ""
  setValidSpecies /\ validSpeciesEvent <- useState (Nothing :: Maybe Boolean)

  setDominantTarpene /\ dominantTarpeneEvent <- useState ""
  setValidDominantTarpene /\ validDominantTarpeneEvent <- useState (Nothing :: Maybe Boolean)

  -- Helper: reset all fields
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
      setTags ""
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
      setTarpenes ""
      setLineage ""

    -- Helper functions for type conversions
    ensureNumber :: String -> String
    ensureNumber str = fromMaybe "0.0" $ map show $ Num.fromString $ trim str

    ensureInt :: String -> String
    ensureInt str = fromMaybe "0" $ map show $ fromString $ trim str

    ensurePercentage :: String -> String
    ensurePercentage str = 
      if String.contains (Pattern "%") str
        then trim str
        else trim str <> "%"

    -- Category options matching BudView's ItemCategory
    categoryOptions = 
      [ { value: "", label: "Select..." }
      , { value: itemCategoryToString Flower, label: itemCategoryToString Flower }
      , { value: itemCategoryToString PreRolls, label: itemCategoryToString PreRolls }
      , { value: itemCategoryToString Vaporizers, label: itemCategoryToString Vaporizers }
      , { value: itemCategoryToString Edibles, label: itemCategoryToString Edibles }
      , { value: itemCategoryToString Drinks, label: itemCategoryToString Drinks }
      , { value: itemCategoryToString Concentrates, label: itemCategoryToString Concentrates }
      , { value: itemCategoryToString Topicals, label: itemCategoryToString Topicals }
      , { value: itemCategoryToString Tinctures, label: itemCategoryToString Tinctures }
      , { value: itemCategoryToString Accessories, label: itemCategoryToString Accessories }
      ]

    -- Updated categoryConfig with exact values from ItemCategory
    categoryConfig' = 
      { label: "Category"
      , options: categoryOptions
      , defaultValue: ""
      }

    -- Determine if the form is valid for enabling the button
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

  -- The main UI
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
        , makeDropdown categoryConfig' setCategory setValidCategory validCategoryEvent
        , makeField descriptionConfig setDescription setValidDescription validDescriptionEvent
        , makeField tagsConfig setTags (const $ pure unit) (pure $ Just true)
        , makeField thcConfig setThc setValidThc validThcEvent
        , makeField cbgConfig setCbg setValidCbg validCbgEvent
        , makeField strainConfig setStrain setValidStrain validStrainEvent
        , makeField creatorConfig setCreator setValidCreator validCreatorEvent
        , makeField speciesConfig setSpecies setValidSpecies validSpeciesEvent
        , makeField dominantTarpeneConfig setDominantTarpene setValidDominantTarpene validDominantTarpeneEvent
        , makeField tarpenesConfig setTarpenes (const $ pure unit) (pure $ Just true)
        , makeField lineageConfig setLineage (const $ pure unit) (pure $ Just true)
        ]
    , D.button
        [ DA.klass_ $ buttonClass "green"
        , DA.disabled $ map show $ (||) <$> submittingEvent <*> map not isFormValid
        , DL.runOn DL.click $ fiber <#> \f -> setFiber =<< launchAff do
            killFiber (error "Cancelling previous submission") f
            liftEffect $ setSubmitting true

            -- Create an event for submission
            { push, event } <- liftEffect $ liftST create

            -- Sample the form values into a proper MenuItemFormInput
            let 
              formDataPoll = 
                (\name sku brand price quantity category description tags
                   thc cbg strain creator species dominant_tarpene tarpenes lineage -> 
                  { name
                  , sku
                  , brand
                  , price: ensureNumber price
                  , quantity: ensureInt quantity
                  , category: trim category  -- Category string comes pre-formatted from dropdown
                  , description
                  , tags
                  , strainLineage:
                      { thc: ensurePercentage thc
                      , cbg: ensurePercentage cbg
                      , strain
                      , creator
                      , species
                      , dominant_tarpene
                      , tarpenes
                      , lineage
                      }
                  } :: MenuItemFormInput
                ) <$> nameEvent
                  <*> skuEvent
                  <*> brandEvent
                  <*> priceEvent
                  <*> quantityEvent
                  <*> categoryEvent
                  <*> descriptionEvent
                  <*> tagsEvent
                  <*> thcEvent
                  <*> cbgEvent
                  <*> strainEvent
                  <*> creatorEvent 
                  <*> speciesEvent 
                  <*> dominantTarpeneEvent
                  <*> tarpenesEvent
                  <*> lineageEvent

              formEvent = sample_ formDataPoll event

            -- Subscribe to form event and handle validation
            sub <- liftEffect $ subscribe formEvent \formInput -> do
              case validateForm formInput of
                Left err -> liftEffect do
                  setStatusMessage $ "Validation error: " <> err
                  setSubmitting false
                
                Right menuItem -> launchAff_ do
                  result <- postInventoryToJson menuItem
                  liftEffect case result of
                    Right (Message msg) -> do
                      setStatusMessage "Item successfully submitted!"
                      resetForm
                    Right (InventoryData _) -> 
                      setStatusMessage "Unexpected response type"
                    Left err -> 
                      setStatusMessage $ "Error submitting item: " <> err
                  liftEffect $ setSubmitting false

            -- Push to trigger form submission
            liftEffect $ push unit
        ]
        [ text $ map (\submitting -> 
            if submitting then "Submitting..." else "Submit"
          ) submittingEvent 
        ]
    , D.div
        [ DA.klass_ "mt-4 text-center" ]
        [ text statusMessageEvent ]
    ]