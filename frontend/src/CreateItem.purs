module CreateItem where

import Prelude

import API (writeInventory)
import Data.Array (all)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Form (brandConfig, buttonClass, categoryConfig, cbgConfig, creatorConfig, descriptionConfig, dominantTerpeneConfig, effectsConfig, imgConfig, leaflyUrlConfig, lineageConfig, makeDropdown, makeField, measureUnitConfig, nameConfig, perPackageConfig, priceConfig, quantityConfig, skuConfig, sortConfig, speciesConfig, strainConfig, subcategoryConfig, tagsConfig, terpenesConfig, thcConfig)
import Types (InventoryResponse(..))
import UUIDGen (genUUID)
import Utils (ensureInt, ensureNumber)
import Validation (validateMenuItem)

createItem :: String -> Nut
createItem initialUUID = Deku.do
  -- Initialize with the UUID string that was passed in
  setSku /\ skuEvent <- useState initialUUID
  setValidSku /\ validSkuEvent <- useState (Just true)

  -- Other state
  setStatusMessage /\ statusMessageEvent <- useState ""
  setSubmitting /\ submittingEvent <- useState false
  setFiber /\ _ <- useState (pure unit)

  setName /\ nameEvent <- useState ""
  setValidName /\ validNameEvent <- useState (Just false)

  setBrand /\ brandEvent <- useState ""
  setValidBrand /\ validBrandEvent <- useState (Just false)

  setPrice /\ priceEvent <- useState ""
  setValidPrice /\ validPriceEvent <- useState (Just false)

  setQuantity /\ quantityEvent <- useState ""
  setValidQuantity /\ validQuantityEvent <- useState (Just false)

  setCategory /\ categoryEvent <- useState ""
  setValidCategory /\ validCategoryEvent <- useState (Just false)

  setDescription /\ descriptionEvent <- useState ""
  setTags /\ tagsEvent <- useState ""
  setEffects /\ effectsEvent <- useState ""
  setTerpenes /\ terpenesEvent <- useState ""
  setLineage /\ lineageEvent <- useState ""

  setThc /\ thcEvent <- useState ""
  setValidThc /\ validThcEvent <- useState (Just false)

  setCbg /\ cbgEvent <- useState ""
  setValidCbg /\ validCbgEvent <- useState (Just false)

  setStrain /\ strainEvent <- useState ""
  setValidStrain /\ validStrainEvent <- useState (Just false)

  setCreator /\ creatorEvent <- useState ""
  setValidCreator /\ validCreatorEvent <- useState (Just false)

  setSpecies /\ speciesEvent <- useState ""
  setValidSpecies /\ validSpeciesEvent <- useState (Just false)

  setDominantTerpene /\ dominantTerpeneEvent <- useState ""
  setValidDominantTerpene /\ validDominantTerpeneEvent <- useState (Just false)

  setSort /\ sortEvent <- useState ""
  setValidSort /\ validSortEvent <- useState (Just false)

  setMeasureUnit /\ measureUnitEvent <- useState ""
  setValidMeasureUnit /\ validMeasureUnitEvent <- useState (Just false)

  setPerPackage /\ perPackageEvent <- useState ""
  setValidPerPackage /\ validPerPackageEvent <- useState (Just false)

  setSubcategory /\ subcategoryEvent <- useState ""
  setValidSubcategory /\ validSubcategoryEvent <- useState (Just false)

  setLeaflyUrl /\ leaflyUrlEvent <- useState ""
  setValidLeaflyUrl /\ validLeaflyUrlEvent <- useState (Just false)

  setImg /\ imgEvent <- useState ""
  setValidImg /\ validImgEvent <- useState (Just false)

  let
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
      vDominantTerpene <- validDominantTerpeneEvent
      in
        all (fromMaybe false)
          [ vName
          , vSku
          , vBrand
          , vPrice
          , vQuantity
          , vCategory
          , vThc
          , vCbg
          , vStrain
          , vCreator
          , vSpecies
          , vDominantTerpene
          ]

  let
    resetForm = do
      newUUID <- genUUID
      setSku (show newUUID)
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
      setDominantTerpene ""
      setValidDominantTerpene (Just false)
      setTerpenes ""
      setLineage ""
      setSort ""
      setValidSort (Just false)
      setMeasureUnit ""
      setValidMeasureUnit (Just false)
      setPerPackage ""
      setValidPerPackage (Just false)
      setSubcategory ""
      setValidSubcategory (Just false)
      setLeaflyUrl ""
      setValidLeaflyUrl (Just false)
      setImg ""
      setValidImg (Just false)

  D.div_
    [
      -- Debug/log the UUID at component load time
      D.div
        [ DA.klass_ "component-loading-debug"
        , DL.load_ \_ -> do
            liftEffect $ Console.log "CreateItem component loading"
            liftEffect $ Console.log $ "Using initialUUID: " <> initialUUID
        ]
        []

    -- The form
    , D.div
        [ DA.klass_ "space-y-4 max-w-2xl mx-auto p-6" ]
        [ D.h2
            [ DA.klass_ "text-2xl font-bold mb-6" ]
            [ text_ "Add New Menu Item" ]
        , makeField (brandConfig "") setBrand setValidBrand validBrandEvent
        , makeField (nameConfig "") setName setValidName validNameEvent
        , makeField (skuConfig initialUUID) setSku setValidSku validSkuEvent
        , makeField (sortConfig "") setSort setValidSort validSortEvent
        , makeField (priceConfig "") setPrice setValidPrice validPriceEvent
        , makeField (quantityConfig "") setQuantity setValidQuantity validQuantityEvent
        , makeField (perPackageConfig "") setPerPackage setValidPerPackage validPerPackageEvent
        , makeField (measureUnitConfig "") setMeasureUnit setValidMeasureUnit validMeasureUnitEvent
        , makeField (subcategoryConfig "") setSubcategory setValidSubcategory validSubcategoryEvent
        , makeDropdown categoryConfig setCategory setValidCategory validCategoryEvent
        , makeField (descriptionConfig "") setDescription (const $ pure unit) (pure $ Just true)
        , makeField (tagsConfig "") setTags (const $ pure unit) (pure $ Just true)
        , makeField (effectsConfig "") setEffects (const $ pure unit) (pure $ Just true)
        , makeField (thcConfig "") setThc setValidThc validThcEvent
        , makeField (cbgConfig "") setCbg setValidCbg validCbgEvent
        , makeDropdown speciesConfig setSpecies setValidSpecies validSpeciesEvent
        , makeField (strainConfig "") setStrain setValidStrain validStrainEvent
        , makeField (dominantTerpeneConfig "") setDominantTerpene setValidDominantTerpene validDominantTerpeneEvent
        , makeField (terpenesConfig "") setTerpenes (const $ pure unit) (pure $ Just true)
        , makeField (lineageConfig "") setLineage (const $ pure unit) (pure $ Just true)
        , makeField (creatorConfig "") setCreator setValidCreator validCreatorEvent
        , makeField (leaflyUrlConfig "") setLeaflyUrl setValidLeaflyUrl validLeaflyUrlEvent
        , makeField (imgConfig "") setImg setValidImg validImgEvent
        ]
    , D.button
        [ DA.klass_ $ buttonClass "green"
        , DA.disabled $ map show $ (||) <$> submittingEvent <*> map not isFormValid
        , DL.runOn DL.click $
            ( \sort name sku brand price measureUnit perPackage quantity category subcategory description tags effects thc cbg strain creator species dominantTerpene terpenes lineage leaflyUrl img -> do
                setSubmitting true
                void $ setFiber =<< launchAff do
                  let
                    formInput =
                      { sort: ensureInt sort
                      , name
                      , sku
                      , brand
                      , price: ensureNumber price
                      , measure_unit: measureUnit
                      , per_package: perPackage
                      , quantity: ensureInt quantity
                      , category
                      , subcategory
                      , description
                      , tags
                      , effects
                      , strain_lineage:
                          { thc
                          , cbg
                          , strain
                          , creator
                          , species
                          , dominant_terpene: dominantTerpene
                          , terpenes
                          , lineage
                          , leafly_url: leaflyUrl
                          , img
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
                      result <- writeInventory menuItem
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
                          Console.error "API Error:"
                          Console.errorShow err
                          setStatusMessage $ "Error saving item: " <> err
                      liftEffect $ Console.groupEnd
                      liftEffect $ setSubmitting false
            ) <$> sortEvent
              <*> nameEvent
              <*> skuEvent
              <*> brandEvent
              <*> priceEvent
              <*> measureUnitEvent
              <*> perPackageEvent
              <*> quantityEvent
              <*> categoryEvent
              <*> subcategoryEvent
              <*> descriptionEvent
              <*> tagsEvent
              <*> effectsEvent
              <*> thcEvent
              <*> cbgEvent
              <*> strainEvent
              <*> creatorEvent
              <*> speciesEvent
              <*> dominantTerpeneEvent
              <*> terpenesEvent
              <*> lineageEvent
              <*> leaflyUrlEvent
              <*> imgEvent
        ]
        [ text $ map
            ( \submitting ->
                if submitting then "Submitting..." else "Submit"
            )
            submittingEvent
        ]
    , D.div
        [ DA.klass_ "mt-4 text-center" ]
        [ text statusMessageEvent ]
    ]
