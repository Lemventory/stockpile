module CreateItem where

import Prelude

import API (postInventoryToJson)
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
import Effect.Class.Console as Console
import FRP.Event (create, subscribe)
import FRP.Poll (sample_)
import Form (MenuItemFormInput, brandConfig, buttonClass, categoryConfig, cbgConfig, creatorConfig, descriptionConfig, dominantTarpeneConfig, lineageConfig, makeDropdown, makeField, nameConfig, priceConfig, quantityConfig, skuConfig, speciesConfig, strainConfig, tagsConfig, tarpenesConfig, thcConfig, validateForm)
import Types (InventoryResponse(..), ItemCategory(..), itemCategoryToString)

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

  setCategory /\ categoryEvent <- useState ""
  setValidCategory /\ validCategoryEvent <- useState (Nothing :: Maybe Boolean)

  setDescription /\ descriptionEvent <- useState ""
  setValidDescription /\ validDescriptionEvent <- useState (Nothing :: Maybe Boolean)

  -- Array fields
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

    ensureNumber :: String -> String
    ensureNumber str = fromMaybe "0.0" $ map show $ Num.fromString $ trim str

    ensureInt :: String -> String
    ensureInt str = fromMaybe "0" $ map show $ fromString $ trim str

    ensurePercentage :: String -> String
    ensurePercentage str = 
      if String.contains (Pattern "%") str
        then trim str
        else trim str <> "%"

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

    categoryConfig' = 
      { label: "Category"
      , options: categoryOptions
      , defaultValue: ""
      }

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

            { push, event } <- liftEffect $ liftST create

            let 
              formDataPoll = 
                (\name sku brand price quantity category description tags
                   thc cbg strain creator species dominant_tarpene tarpenes lineage -> 
                  { name
                  , sku
                  , brand
                  , price: ensureNumber price
                  , quantity: ensureInt quantity
                  , category: trim category
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

            void $ liftEffect $ subscribe formEvent \formInput -> launchAff_ do
              liftEffect $ Console.group "Form Submission"
              liftEffect $ Console.logShow formInput
              liftEffect $ Console.info "Category value before validation:"
              liftEffect $ Console.logShow formInput.category
              
              case validateForm formInput of
                Left err -> do
                  liftEffect $ Console.error "Form validation failed:"
                  liftEffect $ Console.errorShow err
                  liftEffect $ Console.groupEnd
                  liftEffect do
                    setStatusMessage $ "Validation error: " <> err
                    setSubmitting false
                
                Right menuItem -> do
                  liftEffect $ Console.info "Form validated successfully:"
                  liftEffect $ Console.logShow menuItem
                  result <- postInventoryToJson menuItem
                  liftEffect case result of
                    Right (Message _) -> do
                      Console.info "Submission successful"
                      setStatusMessage "Item successfully submitted!"
                      resetForm
                    Right (InventoryData _) -> do
                      Console.warn "Unexpected response type"
                      setStatusMessage "Unexpected response type"
                    Left err -> do
                      Console.error "API Error:"
                      Console.errorShow err
                      setStatusMessage $ "Error submitting item: " <> err
                  liftEffect $ Console.groupEnd
                  liftEffect $ setSubmitting false

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