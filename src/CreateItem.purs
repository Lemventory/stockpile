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
import Form (MenuItemFormInput, brandConfig, buttonClass, categoryConfig, cbgConfig, creatorConfig, descriptionConfig, dominantTarpeneConfig, effectsConfig, lineageConfig, makeDropdown, makeField, nameConfig, priceConfig, quantityConfig, skuConfig, speciesConfig, strainConfig, tagsConfig, tarpenesConfig, thcConfig, validateForm)
import Types (InventoryResponse(..), genUUID)

-- Type to hold initial form values
type InitialFormValues = 
  { name :: String
  , sku :: String
  , brand :: String
  , price :: String
  , quantity :: String
  , category :: String
  , description :: String
  , tags :: String
  , effects :: String
  , thc :: String
  , cbg :: String
  , strain :: String
  , creator :: String
  , species :: String
  , dominantTarpene :: String
  , tarpenes :: String
  , lineage :: String
  }

defaultFormValues :: Effect InitialFormValues
defaultFormValues = do
  initialId <- genUUID
  pure
    { name: "scapula"
    , sku: show initialId
    , brand: "scapula"
    , price: "3.33"
    , quantity: "3"
    , category: "Flower"
    , description: "scapula"
    , tags: "here, are, your, tags"
    , effects: "relaxed, happy, sleepy"
    , thc: "24.5%"
    , cbg: "1.0%"
    , strain: "you know"
    , creator: "fuckers"
    , species: "Indica"
    , dominantTarpene: "scapula"
    , tarpenes: "scapula, rapula, dignity"
    , lineage: "dunno, yo"
    }

createItem :: Effect Unit
createItem = do
  initialValues <- defaultFormValues

  void $ runInBody Deku.do
    -- Status and loading state
    setStatusMessage /\ statusMessageEvent <- useState ""
    setSubmitting /\ submittingEvent <- useState false
    setFiber /\ fiber <- useState (pure unit)

    -- Form fields
    setName /\ nameEvent <- useState initialValues.name
    setValidName /\ validNameEvent <- useState (Just true)

    setSku /\ skuEvent <- useState initialValues.sku
    setValidSku /\ validSkuEvent <- useState (Just true)

    setBrand /\ brandEvent <- useState initialValues.brand
    setValidBrand /\ validBrandEvent <- useState (Just true)

    setPrice /\ priceEvent <- useState initialValues.price
    setValidPrice /\ validPriceEvent <- useState (Just true)

    setQuantity /\ quantityEvent <- useState initialValues.quantity
    setValidQuantity /\ validQuantityEvent <- useState (Just true)

    setCategory /\ categoryEvent <- useState initialValues.category
    setValidCategory /\ validCategoryEvent <- useState (Just true)

    setDescription /\ descriptionEvent <- useState initialValues.description
    setValidDescription /\ validDescriptionEvent <- useState (Just true)

    -- Array fields
    setTags /\ tagsEvent <- useState initialValues.tags
    setEffects /\ effectsEvent <- useState initialValues.effects
    setTarpenes /\ tarpenesEvent <- useState initialValues.tarpenes
    setLineage /\ lineageEvent <- useState initialValues.lineage

    -- StrainLineage fields
    setThc /\ thcEvent <- useState initialValues.thc
    setValidThc /\ validThcEvent <- useState (Just true)

    setCbg /\ cbgEvent <- useState initialValues.cbg
    setValidCbg /\ validCbgEvent <- useState (Just true)

    setStrain /\ strainEvent <- useState initialValues.strain
    setValidStrain /\ validStrainEvent <- useState (Just true)

    setCreator /\ creatorEvent <- useState initialValues.creator
    setValidCreator /\ validCreatorEvent <- useState (Just true)

    setSpecies /\ speciesEvent <- useState initialValues.species
    setValidSpecies /\ validSpeciesEvent <- useState (Just true)

    setDominantTarpene /\ dominantTarpeneEvent <- useState initialValues.dominantTarpene
    setValidDominantTarpene /\ validDominantTarpeneEvent <- useState (Just true)

    let
      -- Field configurations
      nameField = nameConfig initialValues.name
      skuField = skuConfig initialValues.sku
      brandField = brandConfig initialValues.brand
      priceField = priceConfig initialValues.price
      quantityField = quantityConfig initialValues.quantity
      descriptionField = descriptionConfig initialValues.description
      tagsField = tagsConfig initialValues.tags
      effectsField = effectsConfig initialValues.effects
      thcField = thcConfig initialValues.thc
      cbgField = cbgConfig initialValues.cbg
      strainField = strainConfig initialValues.strain
      creatorField = creatorConfig initialValues.creator
      dominantTarpeneField = dominantTarpeneConfig initialValues.dominantTarpene
      tarpenesField = tarpenesConfig initialValues.tarpenes
      lineageField = lineageConfig initialValues.lineage

      resetForm = do
        newId <- genUUID
        setName ""
        setValidName Nothing
        setSku (show newId)
        setValidSku (Just true)
        setBrand ""
        setValidBrand (Just true)
        setPrice ""
        setValidPrice (Just true)
        setQuantity ""
        setValidQuantity (Just true)
        setCategory ""
        setValidCategory (Just true)
        setDescription ""
        setValidDescription (Just true)
        setTags ""
        setEffects ""
        setThc "24.5%"
        setValidThc (Just true)
        setCbg "1.0%"
        setValidCbg (Just true)
        setStrain ""
        setValidStrain (Just true)
        setCreator ""
        setValidCreator (Just true)
        setSpecies ""
        setValidSpecies (Just true)
        setDominantTarpene ""
        setValidDominantTarpene (Just true)
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

    -- Render form
    D.div_
      [ D.div
          [ DA.klass_ "space-y-4 max-w-2xl mx-auto p-6" ]
          [ D.h2 
              [ DA.klass_ "text-2xl font-bold mb-6" ]
              [ text_ "Add New Menu Item" ]
          , makeField nameField setName setValidName validNameEvent
          , makeField skuField setSku setValidSku validSkuEvent
          , makeField brandField setBrand setValidBrand validBrandEvent
          , makeField priceField setPrice setValidPrice validPriceEvent
          , makeField quantityField setQuantity setValidQuantity validQuantityEvent
          , D.div_
            [ makeDropdown categoryConfig setCategory setValidCategory validCategoryEvent
            , D.div
                [ DA.klass_ "mt-2 text-sm text-gray-500" ]
                [ text $ map (\cat -> "Selected category: " <> cat) categoryEvent ]
            ]
          , makeField descriptionField setDescription setValidDescription validDescriptionEvent
          , makeField tagsField setTags (const $ pure unit) (pure $ Just true)
          , makeField effectsField setEffects (const $ pure unit) (pure $ Just true)
          , makeField thcField setThc setValidThc validThcEvent
          , makeField cbgField setCbg setValidCbg validCbgEvent
          , makeField strainField setStrain setValidStrain validStrainEvent
          , makeField creatorField setCreator setValidCreator validCreatorEvent
          , makeDropdown speciesConfig setSpecies setValidSpecies validSpeciesEvent
          , makeField dominantTarpeneField setDominantTarpene setValidDominantTarpene validDominantTarpeneEvent
          , makeField tarpenesField setTarpenes (const $ pure unit) (pure $ Just true)
          , makeField lineageField setLineage (const $ pure unit) (pure $ Just true)
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
                  (\name sku brand price quantity category description tags effects
                     thc cbg strain creator species dominant_tarpene tarpenes lineage -> 
                    { name
                    , sku
                    , brand
                    , price: ensureNumber price
                    , quantity: ensureInt quantity
                    , category: trim category
                    , description
                    , tags
                    , effects
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
                    <*> effectsEvent
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
                liftEffect $ Console.log "Full form data:"
                liftEffect $ Console.logShow formInput
                
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