module EditItem where

import Prelude

import API (readInventory, writeInventory)
import Data.Array (all, find)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners (runOn)
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Form (brandConfig, buttonClass, categoryConfig, cbgConfig, creatorConfig, descriptionConfig, dominantTerpeneConfig, effectsConfig, imgConfig, leaflyUrlConfig, lineageConfig, makeDropdown, makeField, measureUnitConfig, nameConfig, perPackageConfig, priceConfig, quantityConfig, skuConfig, sortConfig, speciesConfig, strainConfig, subcategoryConfig, tagsConfig, terpenesConfig, thcConfig)
import Types (Inventory(..), InventoryResponse(..), MenuItem(..), StrainLineage(..))
import Utils (ensureInt, ensureNumber)
import Validation (validateMenuItem)

data UIState
  = Beginning
  | Loading
  | Loaded MenuItem
  | Error String

editItem :: String -> Nut
editItem targetUUID = Deku.do
  setUIState /\ uiStateEvent <- useState Beginning
  setStatusMessage /\ statusMessageEvent <- useState ""
  setSubmitting /\ submittingEvent <- useState false
  setFiber /\ _ <- useState (pure unit)

  setName /\ nameEvent <- useState ""
  setValidName /\ validNameEvent <- useState (Just false)

  setSku /\ skuEvent <- useState targetUUID
  setValidSku /\ validSkuEvent <- useState (Just true)

  setBrand /\ brandEvent <- useState ""
  setValidBrand /\ validBrandEvent <- useState (Just false)

  setPrice /\ priceEvent <- useState ""
  setValidPrice /\ validPriceEvent <- useState (Just false)

  setQuantity /\ quantityEvent <- useState ""
  setValidQuantity /\ validQuantityEvent <- useState (Just false)

  setSort /\ sortEvent <- useState ""
  setValidSort /\ validSortEvent <- useState (Just false)

  setMeasureUnit /\ measureUnitEvent <- useState ""
  setValidMeasureUnit /\ validMeasureUnitEvent <- useState (Just false)

  setPerPackage /\ perPackageEvent <- useState ""
  setValidPerPackage /\ validPerPackageEvent <- useState (Just false)

  setCategory /\ categoryEvent <- useState ""
  setValidCategory /\ validCategoryEvent <- useState (Just false)

  setSubcategory /\ subcategoryEvent <- useState ""
  setValidSubcategory /\ validSubcategoryEvent <- useState (Just false)

  setDescription /\ descriptionEvent <- useState ""
  setTags /\ tagsEvent <- useState ""
  setEffects /\ effectsEvent <- useState ""

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

  setTerpenes /\ terpenesEvent <- useState ""
  setLineage /\ lineageEvent <- useState ""

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
      vSort <- validSortEvent
      vMeasureUnit <- validMeasureUnitEvent
      vPerPackage <- validPerPackageEvent
      vCategory <- validCategoryEvent
      vSubcategory <- validSubcategoryEvent
      vThc <- validThcEvent
      vCbg <- validCbgEvent
      vStrain <- validStrainEvent
      vCreator <- validCreatorEvent
      vSpecies <- validSpeciesEvent
      vDominantTerpene <- validDominantTerpeneEvent
      vLeaflyUrl <- validLeaflyUrlEvent
      vImg <- validImgEvent
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
          , vDominantTerpene
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
      setDominantTerpene meta.dominant_terpene
      setValidDominantTerpene (Just true)
      setTerpenes (joinWith ", " meta.terpenes)
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
                void $ launchAff do
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
        , uiStateEvent <#~> case _ of
            Beginning -> text_ "Item not found"
            Loading -> text_ "Loading..."
            Error msg -> text_ $ "Error: " <> msg
            Loaded _ -> D.div_
              [ D.h2
                  [ DA.klass_ "text-2xl font-bold mb-6" ]
                  [ text_ "Edit Menu Item" ]
              , makeField (nameConfig "") setName setValidName validNameEvent
              , makeField (skuConfig targetUUID) setSku setValidSku validSkuEvent
              , makeField (brandConfig "") setBrand setValidBrand validBrandEvent
              , makeField (priceConfig "") setPrice setValidPrice validPriceEvent
              , makeField (quantityConfig "") setQuantity setValidQuantity validQuantityEvent
              , makeField (sortConfig "") setSort setValidSort validSortEvent
              , makeField (measureUnitConfig "") setMeasureUnit setValidMeasureUnit validMeasureUnitEvent
              , makeField (perPackageConfig "") setPerPackage setValidPerPackage validPerPackageEvent
              , makeField (subcategoryConfig "") setSubcategory setValidSubcategory validSubcategoryEvent
              , makeDropdown categoryConfig setCategory setValidCategory validCategoryEvent
              , makeField (descriptionConfig "") setDescription (const $ pure unit) (pure $ Just true)
              , makeField (tagsConfig "") setTags (const $ pure unit) (pure $ Just true)
              , makeField (effectsConfig "") setEffects (const $ pure unit) (pure $ Just true)
              , makeField (thcConfig "") setThc setValidThc validThcEvent
              , makeField (cbgConfig "") setCbg setValidCbg validCbgEvent
              , makeField (strainConfig "") setStrain setValidStrain validStrainEvent
              , makeField (creatorConfig "") setCreator setValidCreator validCreatorEvent
              , makeDropdown speciesConfig setSpecies setValidSpecies validSpeciesEvent
              , makeField (dominantTerpeneConfig "") setDominantTerpene setValidDominantTerpene validDominantTerpeneEvent
              , makeField (terpenesConfig "") setTerpenes (const $ pure unit) (pure $ Just true)
              , makeField (lineageConfig "") setLineage (const $ pure unit) (pure $ Just true)
              , makeField (leaflyUrlConfig "") setLeaflyUrl setValidLeaflyUrl validLeaflyUrlEvent
              , makeField (imgConfig "") setImg setValidImg validImgEvent
              ]
        , D.button
            [ DA.klass_ $ buttonClass "green"
            , DA.disabled $ map show $ (||) <$> submittingEvent <*> map not isFormValid
            , runOn DL.click $
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
                            Right (InventoryData _) -> do
                              Console.info "Item updated in inventory"
                              setStatusMessage "Item successfully updated!"
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
                (\isSubmitting -> if isSubmitting then "Submitting..." else "Submit")
                submittingEvent
            ]
        , D.div
            [ DA.klass_ "mt-4 text-center" ]
            [ text statusMessageEvent ]
        ]
    ]
