module CreateItem where

import Prelude

import Types (InventoryResponse(..))
import UUID (genUUID)
import Validation (validateMenuItem)
import Form (buttonClass, makeDropdown, makeField, brandConfig, categoryConfig, cbgConfig, creatorConfig, descriptionConfig, dominantTarpeneConfig, effectsConfig, lineageConfig, nameConfig, priceConfig, quantityConfig, skuConfig, speciesConfig, strainConfig, tagsConfig, tarpenesConfig, thcConfig)
import API (updateInventoryInJson)

import Data.Array (all)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String (trim)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console

createItem :: Effect Unit
createItem = do
  initialId <- genUUID
  let initialSkuStr = show initialId
  
  void $ runInBody Deku.do
    -- Status and loading state
    setStatusMessage /\ statusMessageEvent <- useState ""
    setSubmitting /\ submittingEvent <- useState false
    setFiber /\ _ <- useState (pure unit)

    -- Initialize states with Just false instead of Nothing
    setName /\ nameEvent <- useState ""
    setValidName /\ validNameEvent <- useState (Just false :: Maybe Boolean)

    setSku /\ skuEvent <- useState initialSkuStr
    setValidSku /\ validSkuEvent <- useState (Just true :: Maybe Boolean)

    setBrand /\ brandEvent <- useState ""
    setValidBrand /\ validBrandEvent <- useState (Just false :: Maybe Boolean)

    setPrice /\ priceEvent <- useState ""
    setValidPrice /\ validPriceEvent <- useState (Just false :: Maybe Boolean)

    setQuantity /\ quantityEvent <- useState ""
    setValidQuantity /\ validQuantityEvent <- useState (Just false :: Maybe Boolean)

    setCategory /\ categoryEvent <- useState ""
    setValidCategory /\ validCategoryEvent <- useState (Just false :: Maybe Boolean)

    -- Optional fields don't need validation states
    setDescription /\ descriptionEvent <- useState ""
    setTags /\ tagsEvent <- useState ""
    setEffects /\ effectsEvent <- useState ""
    setTarpenes /\ tarpenesEvent <- useState ""
    setLineage /\ lineageEvent <- useState ""

    -- StrainLineage fields with initial validation states
    setThc /\ thcEvent <- useState ""
    setValidThc /\ validThcEvent <- useState (Just false :: Maybe Boolean)

    setCbg /\ cbgEvent <- useState ""
    setValidCbg /\ validCbgEvent <- useState (Just false :: Maybe Boolean)

    setStrain /\ strainEvent <- useState ""
    setValidStrain /\ validStrainEvent <- useState (Just false :: Maybe Boolean)

    setCreator /\ creatorEvent <- useState ""
    setValidCreator /\ validCreatorEvent <- useState (Just false :: Maybe Boolean)

    setSpecies /\ speciesEvent <- useState ""
    setValidSpecies /\ validSpeciesEvent <- useState (Just false :: Maybe Boolean)

    setDominantTarpene /\ dominantTarpeneEvent <- useState ""
    setValidDominantTarpene /\ validDominantTarpeneEvent <- useState (Just false :: Maybe Boolean)

    let
      -- Form validation check (excluding optional fields)
      isFormValid = ado
        vName <- validNameEvent
        vSku <- validSkuEvent
        vBrand <- validBrandEvent
        vPrice <- validPriceEvent
        vQuantity <- validQuantityEvent
        vCategory <- validCategoryEvent
        vThc <- validThcEvent
        vCbg <- validCbgEvent
        vStrain <- validStrainEvent
        vCreator <- validCreatorEvent
        vSpecies <- validSpeciesEvent
        vDominantTarpene <- validDominantTarpeneEvent
        in all (fromMaybe false)
          [ vName, vSku, vBrand, vPrice, vQuantity, vCategory
          , vThc, vCbg, vStrain, vCreator, vSpecies
          , vDominantTarpene
          ]

      resetForm = do
        newId <- genUUID
        setSku (show newId)
        setValidSku (Just true)
        setName ""
        setValidName (Just false)
        setBrand ""
        setValidBrand (Just false)
        setPrice ""
        setValidPrice (Just false)
        setQuantity ""
        setValidQuantity (Just false)
        setCategory ""
        setValidCategory (Just false)
        setDescription ""
        setTags ""
        setEffects ""
        setThc ""
        setValidThc (Just false)
        setCbg ""
        setValidCbg (Just false)
        setStrain ""
        setValidStrain (Just false)
        setCreator ""
        setValidCreator (Just false)
        setSpecies ""
        setValidSpecies (Just false)
        setDominantTarpene ""
        setValidDominantTarpene (Just false)
        setTarpenes ""
        setLineage ""

      ensureNumber :: String -> String
      ensureNumber str = fromMaybe "0.0" $ map show $ Number.fromString $ trim str

      ensureInt :: String -> String
      ensureInt str = fromMaybe "0" $ map show $ fromString $ trim str

    D.div_
      [ D.div
          [ DA.klass_ "space-y-4 max-w-2xl mx-auto p-6" ]
          [ D.h2 
              [ DA.klass_ "text-2xl font-bold mb-6" ]
              [ text_ "Add New Menu Item" ]
          , makeField (nameConfig "") setName setValidName validNameEvent
          , makeField (skuConfig initialSkuStr) setSku setValidSku validSkuEvent
          , makeField (brandConfig "") setBrand setValidBrand validBrandEvent
          , makeField (priceConfig "") setPrice setValidPrice validPriceEvent
          , makeField (quantityConfig "") setQuantity setValidQuantity validQuantityEvent
          , makeDropdown categoryConfig setCategory setValidCategory validCategoryEvent
          , makeField (descriptionConfig "") setDescription (const $ pure unit) (pure $ Just true)
          , makeField (tagsConfig "") setTags (const $ pure unit) (pure $ Just true)
          , makeField (effectsConfig "") setEffects (const $ pure unit) (pure $ Just true)
          , makeField (thcConfig "") setThc setValidThc validThcEvent
          , makeField (cbgConfig "") setCbg setValidCbg validCbgEvent
          , makeField (strainConfig "") setStrain setValidStrain validStrainEvent
          , makeField (creatorConfig "") setCreator setValidCreator validCreatorEvent
          , makeDropdown speciesConfig setSpecies setValidSpecies validSpeciesEvent
          , makeField (dominantTarpeneConfig "") setDominantTarpene setValidDominantTarpene validDominantTarpeneEvent
          , makeField (tarpenesConfig "") setTarpenes (const $ pure unit) (pure $ Just true)
          , makeField (lineageConfig "") setLineage (const $ pure unit) (pure $ Just true)
          ]
      , D.button
          [ DA.klass_ $ buttonClass "green"
          , DA.disabled $ map show $ (||) <$> submittingEvent <*> map not isFormValid
          , DL.runOn DL.click $ 
              (\name sku brand price quantity category description tags effects 
                thc cbg strain creator species dominant_tarpene tarpenes lineage -> do
                  setSubmitting true
                  void $ setFiber =<< launchAff do
                    let formInput = 
                          { name
                          , sku
                          , brand
                          , price: ensureNumber price
                          , quantity: ensureInt quantity
                          , category
                          , description
                          , tags
                          , effects
                          , strainLineage:
                              { thc
                              , cbg
                              , strain
                              , creator
                              , species
                              , dominant_tarpene
                              , tarpenes
                              , lineage
                              }
                          }
                    
                    liftEffect $ Console.group "Form Submission"
                    liftEffect $ Console.log "Form data:"
                    liftEffect $ Console.logShow formInput
                    
                    case validateMenuItem formInput of
                      Left err -> liftEffect do
                        Console.error "Form validation failed:"
                        Console.errorShow err
                        Console.groupEnd
                        setStatusMessage $ "Validation error: " <> err
                        setSubmitting false
                      
                      Right menuItem -> do
                        liftEffect $ Console.info "Form validated successfully:"
                        liftEffect $ Console.logShow menuItem
                        result <- updateInventoryInJson menuItem
                        liftEffect case result of
                          Right (Message msg) -> do
                            Console.info "Submission successful"
                            setStatusMessage msg
                            resetForm
                          Right (InventoryData _) -> do
                            Console.info "Item added to inventory"
                            setStatusMessage "Item successfully added to inventory!"
                            resetForm
                          Left err -> do
                            Console.error "File System Error:"
                            Console.errorShow err
                            setStatusMessage $ "Error saving item: " <> err
                        liftEffect $ Console.groupEnd
                        liftEffect $ setSubmitting false
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
          ]
          [ text $ map (\submitting -> 
              if submitting then "Submitting..." else "Submit"
            ) submittingEvent 
          ]
      , D.div
          [ DA.klass_ "mt-4 text-center" ]
          [ text statusMessageEvent ]
      ]