module CreateItem where

import Prelude

import API (writeInventory)
import Data.Array (all, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V, invalid, toEither)
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
import Types (InventoryResponse(..), MenuItem(..), StrainLineage(..), ItemCategory(..), Species(..))
import Types.UUID (parseUUID)
import UUIDGen (genUUID)
import Utils (ensureInt, ensureNumber, parseCommaList)
import Validation (validateMenuItem)

-- Helper function for parsing ItemCategory
parseCategory :: String -> V (Array String) ItemCategory
parseCategory str = case str of
  "Flower" -> pure Flower
  "PreRolls" -> pure PreRolls
  "Vaporizers" -> pure Vaporizers
  "Edibles" -> pure Edibles
  "Drinks" -> pure Drinks
  "Concentrates" -> pure Concentrates
  "Topicals" -> pure Topicals
  "Tinctures" -> pure Tinctures
  "Accessories" -> pure Accessories
  _ -> invalid ["Invalid category"]

-- Helper function for parsing Species
parseSpecies :: String -> V (Array String) Species  
parseSpecies str = case str of
  "Indica" -> pure Indica
  "IndicaDominantHybrid" -> pure IndicaDominantHybrid
  "Hybrid" -> pure Hybrid
  "SativaDominantHybrid" -> pure SativaDominantHybrid
  "Sativa" -> pure Sativa
  _ -> invalid ["Invalid species"]

createItem :: String -> Nut
createItem initialUUID = Deku.do
  -- State for form values
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
  setDominantTarpene /\ dominantTarpeneValue <- useState ""
  setTerpenes /\ terpenesValue <- useState ""
  setLineage /\ lineageValue <- useState ""
  setSort /\ sortValue <- useState ""
  setMeasureUnit /\ measureUnitValue <- useState ""
  setPerPackage /\ perPackageValue <- useState ""
  setSubcategory /\ subcategoryValue <- useState ""
  setLeaflyUrl /\ leaflyUrlValue <- useState ""
  setImg /\ imgValue <- useState ""

  -- validation states
  setValidSku /\ validSkuValue <- useState (Just true)
  setValidName /\ validNameValue <- useState (Just false)
  setValidBrand /\ validBrandValue <- useState (Just false)
  setValidPrice /\ validPriceValue <- useState (Just false)
  setValidQuantity /\ validQuantityValue <- useState (Just false)
  setValidCategory /\ validCategoryValue <- useState (Just false)
  setValidThc /\ validThcValue <- useState (Just false)
  setValidCbg /\ validCbgValue <- useState (Just false)
  setValidStrain /\ validStrainValue <- useState (Just false)
  setValidCreator /\ validCreatorValue <- useState (Just false)
  setValidSpecies /\ validSpeciesValue <- useState (Just false)
  setValidDominantTarpene /\ validDominantTarpeneValue <- useState (Just false)
  setValidSort /\ validSortValue <- useState (Just false)
  setValidMeasureUnit /\ validMeasureUnitValue <- useState (Just false)
  setValidPerPackage /\ validPerPackageValue <- useState (Just false)
  setValidSubcategory /\ validSubcategoryValue <- useState (Just false)
  setValidLeaflyUrl /\ validLeaflyUrlValue <- useState (Just false)
  setValidImg /\ validImgValue <- useState (Just false)

  -- UI state
  setStatusMessage /\ statusMessageValue <- useState ""
  setSubmitting /\ submittingValue <- useState false
  setErrors /\ errorsValue <- useState []
  setFiber /\ _ <- useState (pure unit)

  -- Form validation logic
  let
    isFormValid = ado
      vName <- validNameValue
      vSku <- validSkuValue
      vBrand <- validBrandValue
      vPrice <- validPriceValue
      vQuantity <- validQuantityValue
      vCategory <- validCategoryValue
      vThc <- validThcValue
      vCbg <- validCbgValue
      vStrain <- validStrainValue
      vCreator <- validCreatorValue
      vSpecies <- validSpeciesValue
      vDominantTarpene <- validDominantTarpeneValue
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
          , vDominantTarpene
          ]

    -- Form reset function
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
      setDominantTarpene ""
      setValidDominantTarpene (Just false)
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
      D.div
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

          , F.makeField (F.brandFieldConfig "") setBrand setValidBrand validSkuValue
          , F.makeField (F.nameFieldConfig "") setName setValidName validNameValue
          , F.makeField (F.skuFieldConfig initialUUID) setSku setValidSku validSkuValue
          , F.makeField (F.sortFieldConfig "") setSort setValidSort validSortValue
          , F.makeField (F.priceFieldConfig "") setPrice setValidPrice validPriceValue
          , F.makeField (F.quantityFieldConfig "") setQuantity setValidQuantity validQuantityValue
          , F.makeField (F.perPackageFieldConfig "") setPerPackage setValidPerPackage validPerPackageValue
          , F.makeField (F.measureUnitFieldConfig "") setMeasureUnit setValidMeasureUnit validMeasureUnitValue
          , F.makeField (F.subcategoryFieldConfig "") setSubcategory setValidSubcategory validSubcategoryValue
          , F.makeDropdown F.categoryConfig setCategory setValidCategory validCategoryValue
          , F.makeField (F.descriptionFieldConfig "") setDescription (const $ pure unit) (pure $ Just true)
          , F.makeField (F.tagsFieldConfig "") setTags (const $ pure unit) (pure $ Just true)
          , F.makeField (F.effectsFieldConfig "") setEffects (const $ pure unit) (pure $ Just true)
          , F.makeField (F.thcFieldConfig "") setThc setValidThc validThcValue
          , F.makeField (F.cbgFieldConfig "") setCbg setValidCbg validCbgValue
          , F.makeDropdown F.speciesConfig setSpecies setValidSpecies validSpeciesValue
          , F.makeField (F.strainFieldConfig "") setStrain setValidStrain validStrainValue
          , F.makeField (F.dominantTerpeneFieldConfig "") setDominantTarpene setValidDominantTarpene validDominantTarpeneValue
          , F.makeField (F.terpenesFieldConfig "") setTerpenes (const $ pure unit) (pure $ Just true)
          , F.makeField (F.lineageFieldConfig "") setLineage (const $ pure unit) (pure $ Just true)
          , F.makeField (F.creatorFieldConfig "") setCreator setValidCreator validCreatorValue
          , F.makeField (F.leaflyUrlFieldConfig "") setLeaflyUrl setValidLeaflyUrl validLeaflyUrlValue
          , F.makeField (F.imgFieldConfig "") setImg setValidImg validImgValue
          ]

      , D.button
          [ DA.klass_ $ F.buttonClass "green"
          , DA.disabled $ map show $ (||) <$> submittingValue <*> map not isFormValid
          , DL.runOn DL.click \_ -> do
              setSubmitting true
              setErrors []
              
              void $ setFiber =<< launchAff do
                let
                  formInput =
                    { sort: ensureInt sortValue
                    , name: nameValue
                    , sku: skuValue
                    , brand: brandValue
                    , price: ensureNumber priceValue
                    , measure_unit: measureUnitValue
                    , per_package: perPackageValue
                    , quantity: ensureInt quantityValue
                    , category: categoryValue
                    , subcategory: subcategoryValue
                    , description: descriptionValue
                    , tags: tagsValue
                    , effects: effectsValue
                    , strain_lineage:
                        { thc: thcValue
                        , cbg: cbgValue
                        , strain: strainValue
                        , creator: creatorValue
                        , species: speciesValue
                        , dominant_terpene: dominantTarpeneValue
                        , terpenes: terpenesValue
                        , lineage: lineageValue
                        , leafly_url: leaflyUrlValue
                        , img: imgValue
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
                    setErrors [err]

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
                        setErrors [err]

                    liftEffect $ Console.groupEnd
                    liftEffect $ setSubmitting false
          ]
          [ text $ map
              (\isSubmitting ->
                  if isSubmitting then "Submitting..." else "Submit"
              )
              submittingValue
          ]

      , D.div
          [ DA.klass_ "mt-4 text-center" ]
          [ text statusMessageValue ]
    ]