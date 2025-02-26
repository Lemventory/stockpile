module CreateItem where

import Prelude

import API (writeInventory)
import Data.Array (all, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Form as F
import Types (InventoryResponse(..))
import UUIDGen (genUUID)
import Utils (ensureInt, ensureNumber)
import Validation (validateMenuItem)

createItem :: String -> Nut
createItem initialUUID = Deku.do

  setSku /\ skuValue <- useState initialUUID
  setName /\ nameValue <- useState ""
  setBrand /\ brandValue <- useState ""
  setPrice /\ priceValue <- useState ""
  setQuantity /\ quantityValue <- useState ""
  setCategory /\ categoryValue <- useState ""
  setDescription /\ descriptionValue <- useState ""
  setTags /\ tagsValue <- useState ""
  setEffects /\ effectsValue <- useState ""
  setThc /\ thcValue <- useState ""
  setCbg /\ cbgValue <- useState ""
  setStrain /\ strainValue <- useState ""
  setCreator /\ creatorValue <- useState ""
  setSpecies /\ speciesValue <- useState ""
  setDominantTerpene /\ dominantTerpeneValue <- useState ""
  setTerpenes /\ terpenesValue <- useState ""
  setLineage /\ lineageValue <- useState ""
  setSort /\ sortValue <- useState ""
  setMeasureUnit /\ measureUnitValue <- useState ""
  setPerPackage /\ perPackageValue <- useState ""
  setSubcategory /\ subcategoryValue <- useState ""
  setLeaflyUrl /\ leaflyUrlValue <- useState ""
  setImg /\ imgValue <- useState ""

  setValidName /\ validNameEvent <- useState (Just false)
  setValidSku /\ validSkuEvent <- useState (Just true)
  setValidBrand /\ validBrandEvent <- useState (Just false)
  setValidPrice /\ validPriceEvent <- useState (Just false)
  setValidQuantity /\ validQuantityEvent <- useState (Just false)
  setValidCategory /\ validCategoryEvent <- useState (Just false)
  setValidThc /\ validThcEvent <- useState (Just false)
  setValidCbg /\ validCbgEvent <- useState (Just false)
  setValidStrain /\ validStrainEvent <- useState (Just false)
  setValidCreator /\ validCreatorEvent <- useState (Just false)
  setValidSpecies /\ validSpeciesEvent <- useState (Just false)
  setValidDominantTerpene /\ validDominantTerpeneEvent <- useState (Just false)
  setValidTerpenes /\ validTerpenesEvent <- useState (Just true)
  setValidLineage /\ validLineageEvent <- useState (Just true)
  setValidSort /\ validSortEvent <- useState (Just false)
  setValidMeasureUnit /\ validMeasureUnitEvent <- useState (Just false)
  setValidPerPackage /\ validPerPackageEvent <- useState (Just false)
  setValidSubcategory /\ validSubcategoryEvent <- useState (Just false)
  setValidLeaflyUrl /\ validLeaflyUrlEvent <- useState (Just false)
  setValidImg /\ validImgEvent <- useState (Just false)
  setValidDescription /\ validDescriptionEvent <- useState (Just true)
  setValidTags /\ validTagsEvent <- useState (Just true)
  setValidEffects /\ validEffectsEvent <- useState (Just true)

  setStatusMessage /\ statusMessageEvent <- useState ""
  setSubmitting /\ submittingEvent <- useState false
  setErrors /\ errorsValue <- useState []
  setFiber /\ _ <- useState (pure unit)

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
      vSort <- validSortEvent
      vMeasureUnit <- validMeasureUnitEvent
      vPerPackage <- validPerPackageEvent
      vSubcategory <- validSubcategoryEvent
      vLeaflyUrl <- validLeaflyUrlEvent
      vImg <- validImgEvent

      vDescription <- validDescriptionEvent
      vTags <- validTagsEvent
      vEffects <- validEffectsEvent
      vTerpenes <- validTerpenesEvent
      vLineage <- validLineageEvent
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
          , vSort
          , vMeasureUnit
          , vPerPackage
          , vSubcategory
          , vLeaflyUrl
          , vImg
          , vDescription
          , vTags
          , vEffects
          , vTerpenes
          , vLineage
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
      setValidDescription (Just true)
      setTags ""
      setValidTags (Just true)
      setEffects ""
      setValidEffects (Just true)
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
      setValidTerpenes (Just true)
      setLineage ""
      setValidLineage (Just true)
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
      setStatusMessage "Form reset successfully"
      setErrors []

  D.div_
    [ D.div
        [ DA.klass_ "component-loading-debug"
        , DL.load_ \_ -> do
            liftEffect $ Console.log "CreateItem component loading"
            liftEffect $ Console.log $ "Using initialUUID: " <> initialUUID
        ]
        []

    , D.div
        [ DA.klass_ "space-y-4 max-w-2xl mx-auto p-6" ]
        [ D.h2
            [ DA.klass_ "text-2xl font-bold mb-6" ]
            [ text_ "Add New Menu Item" ]

        , D.div
            [ DA.klass_ "error-container mb-4" ]
            [ errorsValue <#~> \errs ->
                if null errs then
                  D.span [] []
                else
                  D.ul [ DA.klass_ "text-red-500 text-sm bg-red-50 p-4 rounded" ]
                    (map (\err -> D.li_ [ text_ err ]) errs)
            ]

        , F.makeField (F.brandConfig "") setBrand setValidBrand validBrandEvent
        , F.makeField (F.nameConfig "") setName setValidName validNameEvent
        , F.makeField (F.skuConfig initialUUID) setSku setValidSku validSkuEvent
        , F.makeField (F.sortConfig "") setSort setValidSort validSortEvent
        , F.makeField (F.priceConfig "") setPrice setValidPrice validPriceEvent
        , F.makeField (F.quantityConfig "") setQuantity setValidQuantity validQuantityEvent
        , F.makeField (F.perPackageConfig "") setPerPackage setValidPerPackage validPerPackageEvent
        , F.makeField (F.measureUnitConfig "") setMeasureUnit setValidMeasureUnit validMeasureUnitEvent
        , F.makeField (F.subcategoryConfig "") setSubcategory setValidSubcategory validSubcategoryEvent
        , F.makeDropdown F.categoryConfig setCategory setValidCategory validCategoryEvent
        , F.makeField (F.descriptionConfig "") setDescription setValidDescription validDescriptionEvent
        , F.makeField (F.tagsConfig "") setTags setValidTags validTagsEvent
        , F.makeField (F.effectsConfig "") setEffects setValidEffects validEffectsEvent
        , F.makeField (F.thcConfig "") setThc setValidThc validThcEvent
        , F.makeField (F.cbgConfig "") setCbg setValidCbg validCbgEvent
        , F.makeDropdown F.speciesConfig setSpecies setValidSpecies validSpeciesEvent
        , F.makeField (F.strainConfig "") setStrain setValidStrain validStrainEvent
        , F.makeField (F.dominantTerpeneConfig "") setDominantTerpene setValidDominantTerpene validDominantTerpeneEvent
        , F.makeField (F.terpenesConfig "") setTerpenes setValidTerpenes validTerpenesEvent
        , F.makeField (F.lineageConfig "") setLineage setValidLineage validLineageEvent
        , F.makeField (F.creatorConfig "") setCreator setValidCreator validCreatorEvent
        , F.makeField (F.leaflyUrlConfig "") setLeaflyUrl setValidLeaflyUrl validLeaflyUrlEvent
        , F.makeField (F.imgConfig "") setImg setValidImg validImgEvent
        ]

    , D.button
        [ DA.klass_ $ F.buttonClass "green"
        , DA.disabled $ map show $ (||) <$> submittingEvent <*> map not isFormValid
        , DL.runOn DL.click $
            ( { sort: _
              , name: _
              , sku: _
              , brand: _
              , price: _
              , measureUnit: _
              , perPackage: _
              , quantity: _
              , category: _
              , subcategory: _
              , description: _
              , tags: _
              , effects: _
              , thc: _
              , cbg: _
              , strain: _
              , creator: _
              , species: _
              , dominantTerpene: _
              , terpenes: _
              , lineage: _
              , leaflyUrl: _
              , img: _
              }
                <$> sortValue
                <*> nameValue
                <*> skuValue
                <*> brandValue
                <*> priceValue
                <*> measureUnitValue
                <*> perPackageValue
                <*> quantityValue
                <*> categoryValue
                <*> subcategoryValue
                <*> descriptionValue
                <*> tagsValue
                <*> effectsValue
                <*> thcValue
                <*> cbgValue
                <*> strainValue
                <*> creatorValue
                <*> speciesValue
                <*> dominantTerpeneValue
                <*> terpenesValue
                <*> lineageValue
                <*> leaflyUrlValue
                <*> imgValue
            ) <#> \values -> do
              setSubmitting true
              setErrors []
              setStatusMessage "Processing form submission..."

              void $ setFiber =<< launchAff do
                let
                  formInput =
                    { sort: ensureInt values.sort
                    , name: values.name
                    , sku: values.sku
                    , brand: values.brand
                    , price: ensureNumber values.price
                    , measure_unit: values.measureUnit
                    , per_package: values.perPackage
                    , quantity: ensureInt values.quantity
                    , category: values.category
                    , subcategory: values.subcategory
                    , description: values.description
                    , tags: values.tags
                    , effects: values.effects
                    , strain_lineage:
                        { thc: values.thc
                        , cbg: values.cbg
                        , strain: values.strain
                        , creator: values.creator
                        , species: values.species
                        , dominant_terpene: values.dominantTerpene
                        , terpenes: values.terpenes
                        , lineage: values.lineage
                        , leafly_url: values.leaflyUrl
                        , img: values.img
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
                    setErrors [ err ]

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
                        setErrors [ err ]

                    liftEffect $ Console.groupEnd
                    liftEffect $ setSubmitting false
        ]
        [ text $ map
            ( \isSubmitting ->
                if isSubmitting then "Submitting..." else "Submit"
            )
            submittingEvent
        ]


    , D.div
        [ DA.klass_ "mt-8 p-4 border rounded bg-gray-50" ]
        [ D.h3
            [ DA.klass_ "text-lg font-semibold mb-2" ]
            [ text_ "Debug Information" ]
        , D.div
            [ DA.klass_ "debug-status mb-2 text-sm" ]
            [ D.strong_ [ text_ "Status: " ]
            , text statusMessageEvent
            ]
        , D.div
            [ DA.klass_ "debug-form-state mb-2 text-sm" ]
            [ D.strong_ [ text_ "Form valid: " ]
            , text $ map show isFormValid
            ]
        , D.div
            [ DA.klass_ "debug-form-valid-items grid grid-cols-3 gap-2 text-xs" ]
            [ validNameEvent <#~> \v -> D.div
                [ DA.klass_ $ "p-1 " <> if fromMaybe false v then "text-green-600" else "text-red-600" ]
                [ text_ $ "Name: " <> show v ]
            , validBrandEvent <#~> \v -> D.div
                [ DA.klass_ $ "p-1 " <> if fromMaybe false v then "text-green-600" else "text-red-600" ]
                [ text_ $ "Brand: " <> show v ]
            , validSkuEvent <#~> \v -> D.div
                [ DA.klass_ $ "p-1 " <> if fromMaybe false v then "text-green-600" else "text-red-600" ]
                [ text_ $ "SKU: " <> show v ]
            , validPriceEvent <#~> \v -> D.div
                [ DA.klass_ $ "p-1 " <> if fromMaybe false v then "text-green-600" else "text-red-600" ]
                [ text_ $ "Price: " <> show v ]
            , validQuantityEvent <#~> \v -> D.div
                [ DA.klass_ $ "p-1 " <> if fromMaybe false v then "text-green-600" else "text-red-600" ]
                [ text_ $ "Quantity: " <> show v ]
            , validCategoryEvent <#~> \v -> D.div
                [ DA.klass_ $ "p-1 " <> if fromMaybe false v then "text-green-600" else "text-red-600" ]
                [ text_ $ "Category: " <> show v ]
            , validThcEvent <#~> \v -> D.div
                [ DA.klass_ $ "p-1 " <> if fromMaybe false v then "text-green-600" else "text-red-600" ]
                [ text_ $ "THC: " <> show v ]
            , validCbgEvent <#~> \v -> D.div
                [ DA.klass_ $ "p-1 " <> if fromMaybe false v then "text-green-600" else "text-red-600" ]
                [ text_ $ "CBG: " <> show v ]
            , validStrainEvent <#~> \v -> D.div
                [ DA.klass_ $ "p-1 " <> if fromMaybe false v then "text-green-600" else "text-red-600" ]
                [ text_ $ "Strain: " <> show v ]
            , validCreatorEvent <#~> \v -> D.div
                [ DA.klass_ $ "p-1 " <> if fromMaybe false v then "text-green-600" else "text-red-600" ]
                [ text_ $ "Creator: " <> show v ]
            , validSpeciesEvent <#~> \v -> D.div
                [ DA.klass_ $ "p-1 " <> if fromMaybe false v then "text-green-600" else "text-red-600" ]
                [ text_ $ "Species: " <> show v ]
            , validDominantTerpeneEvent <#~> \v -> D.div
                [ DA.klass_ $ "p-1 " <> if fromMaybe false v then "text-green-600" else "text-red-600" ]
                [ text_ $ "Dom Terpene: " <> show v ]
            ]
        ]
    ]