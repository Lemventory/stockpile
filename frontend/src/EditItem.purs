module EditItem where

import Prelude

import API (readInventory, writeInventory)
import Data.Array (all, find)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners (runOn)
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Form (brandConfig, buttonClass, categoryConfig, cbgConfig, creatorConfig, descriptionConfig, dominantTarpeneConfig, effectsConfig, imgConfig, leaflyUrlConfig, lineageConfig, makeDropdown, makeField, measureUnitConfig, nameConfig, perPackageConfig, priceConfig, quantityConfig, skuConfig, sortConfig, speciesConfig, strainConfig, subcategoryConfig, tagsConfig, tarpenesConfig, thcConfig)
import Types (Inventory(..), InventoryResponse(..), MenuItem(..), StrainLineage(..))
import Utils (ensureInt, ensureNumber)
import Validation (validateMenuItem)

data UIState
  = Beginning
  | Loading
  | Loaded MenuItem
  | Error String

editItem :: String -> Effect Unit
editItem targetUUID = void $ runInBody Deku.do
  setUIState /\ uiState <- useState Beginning
  setStatusMessage /\ statusMessageEvent <- useState ""
  setSubmitting /\ submittingEvent <- useState false
  setFiber /\ _ <- useState (pure unit)

  setName /\ nameEvent <- useState ""
  setValidName /\ validName <- useState (Just false)

  setSku /\ skuEvent <- useState targetUUID
  setValidSku /\ validSku <- useState (Just true)

  setBrand /\ brandEvent <- useState ""
  setValidBrand /\ validBrand <- useState (Just false)

  setPrice /\ priceEvent <- useState ""
  setValidPrice /\ validPrice <- useState (Just false)

  setQuantity /\ quantityEvent <- useState ""
  setValidQuantity /\ validQuantity <- useState (Just false)

  setSort /\ sortEvent <- useState ""
  setValidSort /\ validSort <- useState (Just false)

  setMeasureUnit /\ measureUnitEvent <- useState ""
  setValidMeasureUnit /\ validMeasureUnit <- useState (Just false)

  setPerPackage /\ perPackageEvent <- useState ""
  setValidPerPackage /\ validPerPackage <- useState (Just false)

  setCategory /\ categoryEvent <- useState ""
  setValidCategory /\ validCategory <- useState (Just false)

  setSubcategory /\ subcategoryEvent <- useState ""
  setValidSubcategory /\ validSubcategory <- useState (Just false)

  setDescription /\ descriptionEvent <- useState ""
  setTags /\ tagsEvent <- useState ""
  setEffects /\ effectsEvent <- useState ""

  setThc /\ thcEvent <- useState ""
  setValidThc /\ validThc <- useState (Just false)

  setCbg /\ cbgEvent <- useState ""
  setValidCbg /\ validCbg <- useState (Just false)

  setStrain /\ strainEvent <- useState ""
  setValidStrain /\ validStrain <- useState (Just false)

  setCreator /\ creatorEvent <- useState ""
  setValidCreator /\ validCreator <- useState (Just false)

  setSpecies /\ speciesEvent <- useState ""
  setValidSpecies /\ validSpecies <- useState (Just false)

  setDominantTarpene /\ dominantTarpeneEvent <- useState ""
  setValidDominantTarpene /\ validDominantTarpene <- useState (Just false)

  setTarpenes /\ tarpenesEvent <- useState ""
  setLineage /\ lineageEvent <- useState ""

  setLeaflyUrl /\ leaflyUrlEvent <- useState ""
  setValidLeaflyUrl /\ validLeaflyUrl <- useState (Just false)

  setImg /\ imgEvent <- useState ""
  setValidImg /\ validImg <- useState (Just false)

  let
    isFormValid = ado
      vName <- validName
      vSku <- validSku
      vBrand <- validBrand
      vPrice <- validPrice
      vQuantity <- validQuantity
      vSort <- validSort
      vMeasureUnit <- validMeasureUnit
      vPerPackage <- validPerPackage
      vCategory <- validCategory
      vSubcategory <- validSubcategory
      vThc <- validThc
      vCbg <- validCbg
      vStrain <- validStrain
      vCreator <- validCreator
      vSpecies <- validSpecies
      vDominantTarpene <- validDominantTarpene
      vLeaflyUrl <- validLeaflyUrl
      vImg <- validImg
      in
        all (fromMaybe false)
          [ vName
          , vSku
          , vBrand
          , vPrice
          , vQuantity
          , vSort
          , vMeasureUnit
          , vPerPackage
          , vCategory
          , vSubcategory
          , vThc
          , vCbg
          , vStrain
          , vCreator
          , vSpecies
          , vDominantTarpene
          , vLeaflyUrl
          , vImg
          ]

    updateFormFields :: MenuItem -> Effect Unit
    updateFormFields (MenuItem item) = do
      let StrainLineage meta = item.strain_lineage
      setName item.name
      setValidName (Just true)
      setSku (show item.sku)
      setValidSku (Just true)
      setBrand item.brand
      setValidBrand (Just true)
      setPrice (show item.price)
      setValidPrice (Just true)
      setCategory (show item.category)
      setValidCategory (Just true)
      setSubcategory item.subcategory
      setValidSubcategory (Just true)
      setQuantity (show item.quantity)
      setValidQuantity (Just true)
      setSort (show item.sort)
      setValidSort (Just true)
      setMeasureUnit item.measure_unit
      setValidMeasureUnit (Just true)
      setPerPackage item.per_package
      setValidPerPackage (Just true)
      setDescription item.description
      setTags (joinWith ", " item.tags)
      setEffects (joinWith ", " item.effects)
      setThc meta.thc
      setValidThc (Just true)
      setCbg meta.cbg
      setValidCbg (Just true)
      setStrain meta.strain
      setValidStrain (Just true)
      setCreator meta.creator
      setValidCreator (Just true)
      setSpecies (show meta.species)
      setValidSpecies (Just true)
      setDominantTarpene meta.dominant_tarpene
      setValidDominantTarpene (Just true)
      setTarpenes (joinWith ", " meta.tarpenes)
      setLineage (joinWith ", " meta.lineage)
      setLeaflyUrl meta.leafly_url
      setValidLeaflyUrl (Just true)
      setImg meta.img
      setValidImg (Just true)

  D.div
    [ DA.klass_ "space-y-4 max-w-2xl mx-auto p-6" ]
    [ D.div_
        [ D.div
            [ DA.klass_ "load-container"
            , DL.load_ \_ -> do
                setUIState Loading
                launchAff_ do
                  result <- readInventory
                  liftEffect case result of
                    Right (InventoryData (Inventory items)) ->
                      case find (\(MenuItem item) -> show item.sku == targetUUID) items of
                        Just menuItem -> do
                          updateFormFields menuItem
                          setUIState $ Loaded menuItem
                        Nothing -> setUIState Beginning
                    Right (Message msg) -> setUIState $ Error msg
                    Left err -> setUIState $ Error err
            ]
            []
        , uiState <#~> case _ of
            Beginning -> text_ "Item not found"
            Loading -> text_ "Loading..."
            Error msg -> text_ $ "Error: " <> msg
            Loaded _ -> D.div_
              [ D.h2
                  [ DA.klass_ "text-2xl font-bold mb-6" ]
                  [ text_ "Edit Menu Item" ]
              , makeField (nameConfig "") setName setValidName validName
              , makeField (skuConfig targetUUID) setSku setValidSku validSku
              , makeField (brandConfig "") setBrand setValidBrand validBrand
              , makeField (priceConfig "") setPrice setValidPrice validPrice
              , makeField (quantityConfig "") setQuantity setValidQuantity validQuantity
              , makeField (sortConfig "") setSort setValidSort validSort
              , makeField (measureUnitConfig "") setMeasureUnit setValidMeasureUnit validMeasureUnit
              , makeField (perPackageConfig "") setPerPackage setValidPerPackage validPerPackage
              , makeField (subcategoryConfig "") setSubcategory setValidSubcategory validSubcategory
              , makeDropdown categoryConfig setCategory setValidCategory validCategory
              , makeField (descriptionConfig "") setDescription (const $ pure unit) (pure $ Just true)
              , makeField (tagsConfig "") setTags (const $ pure unit) (pure $ Just true)
              , makeField (effectsConfig "") setEffects (const $ pure unit) (pure $ Just true)
              , makeField (thcConfig "") setThc setValidThc validThc
              , makeField (cbgConfig "") setCbg setValidCbg validCbg
              , makeField (strainConfig "") setStrain setValidStrain validStrain
              , makeField (creatorConfig "") setCreator setValidCreator validCreator
              , makeDropdown speciesConfig setSpecies setValidSpecies validSpecies
              , makeField (dominantTarpeneConfig "") setDominantTarpene setValidDominantTarpene validDominantTarpene
              , makeField (tarpenesConfig "") setTarpenes (const $ pure unit) (pure $ Just true)
              , makeField (lineageConfig "") setLineage (const $ pure unit) (pure $ Just true)
              , makeField (leaflyUrlConfig "") setLeaflyUrl setValidLeaflyUrl validLeaflyUrl
              , makeField (imgConfig "") setImg setValidImg validImg

              ]
        , D.button
            [ DA.klass_ $ buttonClass "green"
            , DA.disabled $ map show $ (||) <$> submittingEvent <*> map not isFormValid
            , runOn DL.click $
                ( \sort name sku brand price measureUnit perPackage quantity category subcategory description tags effects thc cbg strain creator species dominantTarpene tarpenes lineage leaflyUrl img -> do
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
                              , dominant_tarpene: dominantTarpene
                              , tarpenes
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
                            -- resetForm
                            Right (InventoryData _) -> do
                              Console.info "Item added to inventory"
                              setStatusMessage "Item successfully added to inventory!"
                            -- resetForm
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
                  <*> dominantTarpeneEvent
                  <*> tarpenesEvent
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
    ]