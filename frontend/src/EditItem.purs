module EditItem where

import Prelude

import API (readInventory, updateInventory)
import Data.Array (all, find)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Effect.Class (liftEffect)
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Deku.DOM.Listeners as DL
import Form (brandConfig, buttonClass, categoryConfig, cbgConfig, creatorConfig, descriptionConfig, dominantTarpeneConfig, effectsConfig, imgConfig, leaflyUrlConfig, lineageConfig, makeDropdown, makeField, measureUnitConfig, nameConfig, perPackageConfig, priceConfig, quantityConfig, skuConfig, sortConfig, speciesConfig, strainConfig, subcategoryConfig, tagsConfig, tarpenesConfig, thcConfig)
import Types (Inventory(..), InventoryResponse(..), MenuItem(..), StrainLineage(..))
import Validation (validateMenuItem)

editItem :: String -> Effect Unit
editItem targetUUID = void $ runInBody Deku.do
  setStatusMessage /\ statusMessageEvent <- useState ""
  setSubmitting /\ submittingEvent <- useState false
  setLoading /\ loadingEvent <- useState true

  setName /\ nameEvent <- useState ""
  setValidName /\ validNameEvent <- useState (Just false :: Maybe Boolean)

  setSku /\ skuEvent <- useState targetUUID
  setValidSku /\ validSkuEvent <- useState (Just true :: Maybe Boolean)

  setBrand /\ brandEvent <- useState ""
  setValidBrand /\ validBrandEvent <- useState (Just false :: Maybe Boolean)

  setPrice /\ priceEvent <- useState ""
  setValidPrice /\ validPriceEvent <- useState (Just false :: Maybe Boolean)

  setQuantity /\ quantityEvent <- useState ""
  setValidQuantity /\ validQuantityEvent <- useState (Just false :: Maybe Boolean)

  setSort /\ sortEvent <- useState ""
  setValidSort /\ validSortEvent <- useState (Just false :: Maybe Boolean)

  setMeasureUnit /\ measureUnitEvent <- useState ""
  setValidMeasureUnit /\ validMeasureUnitEvent <- useState (Just false :: Maybe Boolean)

  setPerPackage /\ perPackageEvent <- useState ""
  setValidPerPackage /\ validPerPackageEvent <- useState (Just false :: Maybe Boolean)

  setCategory /\ categoryEvent <- useState ""
  setValidCategory /\ validCategoryEvent <- useState (Just false :: Maybe Boolean)

  setSubcategory /\ subcategoryEvent <- useState ""
  setValidSubcategory /\ validSubcategoryEvent <- useState (Just false :: Maybe Boolean)

  setDescription /\ descriptionEvent <- useState ""
  setTags /\ tagsEvent <- useState ""
  setEffects /\ effectsEvent <- useState ""
  setTarpenes /\ tarpenesEvent <- useState ""
  setLineage /\ lineageEvent <- useState ""

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

  setLeaflyUrl /\ leaflyUrlEvent <- useState ""
  setValidLeaflyUrl /\ validLeaflyUrlEvent <- useState (Just false :: Maybe Boolean)

  setImg /\ imgEvent <- useState ""
  setValidImg /\ validImgEvent <- useState (Just false :: Maybe Boolean)

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
      vDominantTarpene <- validDominantTarpeneEvent
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
          , vDominantTarpene
          , vLeaflyUrl
          , vImg
          ]

    handleLoadResult :: Either String InventoryResponse -> Effect Unit
    handleLoadResult = case _ of
      Right (InventoryData (Inventory items)) ->
        case find (\(MenuItem item) -> show item.sku == targetUUID) items of
          Just (MenuItem item) -> do
            Console.log "Found item, loading data..."
            let StrainLineage meta = item.strain_lineage

            setName item.name
            setValidName (Just true)
            setSku (show item.sku)
            setValidSku (Just true)
            setBrand item.brand
            setValidBrand (Just true)
            setPrice (show item.price)
            setValidPrice (Just true)
            setQuantity (show item.quantity)
            setValidQuantity (Just true)
            setSort (show item.sort)
            setValidSort (Just true)
            setMeasureUnit item.measure_unit
            setValidMeasureUnit (Just true)
            setPerPackage item.per_package
            setValidPerPackage (Just true)
            setCategory (show item.category)
            setValidCategory (Just true)
            setSubcategory item.subcategory
            setValidSubcategory (Just true)
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
            setLoading false
          Nothing -> do
            Console.error "Item not found"
            setStatusMessage "Item not found"
            setLoading false
      Right (Message msg) -> do
        Console.log $ "Got message: " <> msg
        setStatusMessage msg
        setLoading false
      Left err -> do
        Console.error $ "Error loading item: " <> err
        setStatusMessage $ "Error loading item: " <> err
        setLoading false

  D.div_
    [ D.div
        [ DA.klass_ "init-load"
        , DL.click_ \_ -> launchAff_ do
            result <- readInventory
            liftEffect $ handleLoadResult result
        ]
        []
    , D.div
        [ DA.klass_ "loading-state" ]
        [ D.div_
            [ text $ map
                (\loading -> if loading then "Loading..." else "")
                loadingEvent
            ]
        ]
    , D.div
        [ DA.klass_ "errors-state" ]
        [ D.div_
            [ text statusMessageEvent ]
        ]
    , D.div
        [ DA.klass_ "space-y-4 max-w-2xl mx-auto p-6" ]
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
        , makeDropdown categoryConfig setCategory setValidCategory validCategoryEvent
        , makeField (subcategoryConfig "") setSubcategory setValidSubcategory validSubcategoryEvent
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
        , makeField (leaflyUrlConfig "") setLeaflyUrl setValidLeaflyUrl validLeaflyUrlEvent
        , makeField (imgConfig "") setImg setValidImg validImgEvent
        ]
    , D.button
        [ DA.klass_ $ buttonClass "green"
        , DA.disabled $ map show $ (||) <$> submittingEvent <*> map not isFormValid
        , DL.runOn DL.click $
            ( \sort name sku brand price measureUnit perPackage quantity category subcategory description tags effects thc cbg strain creator species dominantTarpene tarpenes lineage leaflyUrl img submitting -> do
                when (not submitting) do
                  setSubmitting true
                  void $ launchAff_ do
                    let
                      formInput =
                        { sort
                        , name
                        , sku
                        , brand
                        , price
                        , measure_unit: measureUnit
                        , per_package: perPackage
                        , quantity
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
                        result <- updateInventory menuItem
                        liftEffect case result of
                          Right (Message msg) -> do
                            Console.info "Update successful"
                            setStatusMessage msg
                          Right (InventoryData _) -> do
                            Console.info "Item updated in inventory"
                            setStatusMessage "Item successfully updated!"
                          Left err -> do
                            Console.error "API Error:"
                            Console.errorShow err
                            setStatusMessage $ "Error updating item: " <> err
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
              <*> submittingEvent
        ]
        [ text $ map
            (\submitting -> if submitting then "Updating..." else "Update")
            submittingEvent
        ]
    , D.div
        [ DA.klass_ "mt-4 text-center" ]
        [ text statusMessageEvent ]
    ]