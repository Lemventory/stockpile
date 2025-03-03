module EditItem where

import Prelude

import API (updateInventory)
import Data.Array (all)
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
import Deku.Hooks (useState)
import Data.String.CodeUnits (length)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Form (brandConfig, buttonClass, categoryConfig, cbgConfig, creatorConfig, descriptionConfig, dominantTerpeneConfig, effectsConfig, imgConfig, leaflyUrlConfig, lineageConfig, makeDropdown, makeField, measureUnitConfig, nameConfig, perPackageConfig, priceConfig, quantityConfig, skuConfig, sortConfig, speciesConfig, strainConfig, subcategoryConfig, tagsConfig, terpenesConfig, thcConfig)
import Types (InventoryResponse(..), MenuItem(..), StrainLineage(..))
import Utils (ensureInt, ensureNumber)
import Validation (validateMenuItem)

editItem :: MenuItem -> Nut
editItem (MenuItem item) = Deku.do
  let (StrainLineage lineage) = item.strain_lineage
  let categoryValue = show item.category
  let speciesValue = show lineage.species

  setStatusMessage /\ statusMessageEvent <- useState ""
  setSubmitting /\ submittingEvent <- useState false
  setFiber /\ _ <- useState (pure unit)

  setName /\ nameEvent <- useState item.name
  setValidName /\ validNameEvent <- useState (Just true)

  setSku /\ skuEvent <- useState (show item.sku)
  setValidSku /\ validSkuEvent <- useState (Just true)

  setBrand /\ brandEvent <- useState item.brand
  setValidBrand /\ validBrandEvent <- useState (Just true)

  setPrice /\ priceEvent <- useState (show item.price)
  setValidPrice /\ validPriceEvent <- useState (Just true)

  setQuantity /\ quantityEvent <- useState (show item.quantity)
  setValidQuantity /\ validQuantityEvent <- useState (Just true)

  setSort /\ sortEvent <- useState (show item.sort)
  setValidSort /\ validSortEvent <- useState (Just true)

  setMeasureUnit /\ measureUnitEvent <- useState item.measure_unit
  setValidMeasureUnit /\ validMeasureUnitEvent <- useState (Just true)

  setPerPackage /\ perPackageEvent <- useState item.per_package
  setValidPerPackage /\ validPerPackageEvent <- useState (Just true)

  setCategory /\ categoryEvent <- useState categoryValue
  setValidCategory /\ validCategoryEvent <- useState (Just true)

  setSubcategory /\ subcategoryEvent <- useState item.subcategory
  setValidSubcategory /\ validSubcategoryEvent <- useState (Just true)

  setDescription /\ descriptionEvent <- useState item.description
  setValidDescription /\ validDescriptionEvent <- useState (Just true)

  setTags /\ tagsEvent <- useState (joinWith ", " item.tags)
  setValidTags /\ validTagsEvent <- useState (Just true)

  setEffects /\ effectsEvent <- useState (joinWith ", " item.effects)
  setValidEffects /\ validEffectsEvent <- useState (Just true)

  setThc /\ thcEvent <- useState lineage.thc
  setValidThc /\ validThcEvent <- useState (Just true)

  setCbg /\ cbgEvent <- useState lineage.cbg
  setValidCbg /\ validCbgEvent <- useState (Just true)

  setStrain /\ strainEvent <- useState lineage.strain
  setValidStrain /\ validStrainEvent <- useState (Just true)

  setCreator /\ creatorEvent <- useState lineage.creator
  setValidCreator /\ validCreatorEvent <- useState (Just true)

  setSpecies /\ speciesEvent <- useState speciesValue
  setValidSpecies /\ validSpeciesEvent <- useState (Just true)

  setDominantTerpene /\ dominantTerpeneEvent <- useState
    lineage.dominant_terpene
  setValidDominantTerpene /\ validDominantTerpeneEvent <- useState (Just true)

  setTerpenes /\ terpenesEvent <- useState (joinWith ", " lineage.terpenes)
  setValidTerpenes /\ validTerpenesEvent <- useState (Just true)

  setLineage /\ lineageEvent <- useState (joinWith ", " lineage.lineage)
  setValidLineage /\ validLineageEvent <- useState (Just true)

  setLeaflyUrl /\ leaflyUrlEvent <- useState lineage.leafly_url
  setValidLeaflyUrl /\ validLeaflyUrlEvent <- useState (Just true)

  setImg /\ imgEvent <- useState lineage.img
  setValidImg /\ validImgEvent <- useState (Just true)

  let
    customCategoryConfig = categoryConfig
      { defaultValue: categoryValue, forNewItem: false }
  let
    customSpeciesConfig = speciesConfig
      { defaultValue: speciesValue, forNewItem: false }
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

  D.div
    [ DA.klass_ "space-y-4 max-w-2xl mx-auto p-6"
    , DL.load_ \_ -> do
        liftEffect $ Console.log $ "EditItem component loaded"
        liftEffect $ Console.log $ "Current category value: " <> categoryValue
        liftEffect $ Console.log $ "Current species value: " <> speciesValue

        -- Debug logging for description
        liftEffect $ Console.log $ "Description value from backend: '"
          <> item.description
          <> "'"
        liftEffect $ Console.log $ "Description length: " <> show
          (length item.description)
        liftEffect $ Console.log $ "Description config defaultValue: '"
          <> (descriptionConfig item.description).defaultValue
          <> "'"
        -- Removed the problematic evalState line

        setCategory categoryValue
        setValidCategory (Just true)

        setSpecies speciesValue
        setValidSpecies (Just true)
    ]
    [ D.h2
        [ DA.klass_ "text-2xl font-bold mb-6" ]
        [ text_ "Edit Menu Item" ]
    , makeField (nameConfig item.name) setName setValidName validNameEvent
    , makeField (skuConfig (show item.sku)) setSku setValidSku validSkuEvent
    , makeField (brandConfig item.brand) setBrand setValidBrand validBrandEvent
    , makeField (priceConfig (show item.price)) setPrice setValidPrice
        validPriceEvent
    , makeField (quantityConfig (show item.quantity)) setQuantity
        setValidQuantity
        validQuantityEvent
    , makeField (sortConfig (show item.sort)) setSort setValidSort
        validSortEvent
    , makeField (measureUnitConfig item.measure_unit) setMeasureUnit
        setValidMeasureUnit
        validMeasureUnitEvent
    , makeField (perPackageConfig item.per_package) setPerPackage
        setValidPerPackage
        validPerPackageEvent
    , makeField (subcategoryConfig item.subcategory) setSubcategory
        setValidSubcategory
        validSubcategoryEvent
    , makeDropdown customCategoryConfig setCategory setValidCategory
        validCategoryEvent

    -- Debug rendering
    , D.div [ DA.klass_ "debug-description-container" ]
        [ D.pre [ DA.klass_ "text-xs text-red-500" ]
            [ text_ $ "Debug - Description from backend: '" <> item.description
                <> "'"
            ]
        ]

    -- Description field
    , makeField (descriptionConfig item.description) setDescription
        setValidDescription
        validDescriptionEvent

    , makeField (tagsConfig (joinWith ", " item.tags)) setTags setValidTags
        validTagsEvent
    , makeField (effectsConfig (joinWith ", " item.effects)) setEffects
        setValidEffects
        validEffectsEvent
    , makeField (thcConfig lineage.thc) setThc setValidThc validThcEvent
    , makeField (cbgConfig lineage.cbg) setCbg setValidCbg validCbgEvent
    , makeField (strainConfig lineage.strain) setStrain setValidStrain
        validStrainEvent
    , makeField (creatorConfig lineage.creator) setCreator setValidCreator
        validCreatorEvent
    , makeDropdown customSpeciesConfig setSpecies setValidSpecies
        validSpeciesEvent
    , makeField (dominantTerpeneConfig lineage.dominant_terpene)
        setDominantTerpene
        setValidDominantTerpene
        validDominantTerpeneEvent
    , makeField (terpenesConfig (joinWith ", " lineage.terpenes)) setTerpenes
        setValidTerpenes
        validTerpenesEvent
    , makeField (lineageConfig (joinWith ", " lineage.lineage)) setLineage
        setValidLineage
        validLineageEvent
    , makeField (leaflyUrlConfig lineage.leafly_url) setLeaflyUrl
        setValidLeaflyUrl
        validLeaflyUrlEvent
    , makeField (imgConfig lineage.img) setImg setValidImg validImgEvent

    -- Submit button
    , D.button
        [ DA.klass_ $ buttonClass "green"
        , DA.disabled $ map show $ (||) <$> submittingEvent <*> map not
            isFormValid
        , runOn DL.click $
            ( \sort
               name
               sku
               brand
               price
               measureUnit
               perPackage
               quantity
               category
               subcategory
               description
               tags
               effects
               thc
               cbg
               strain
               creator
               species
               dominantTerpene
               terpenes
               lineage
               leaflyUrl
               img -> do

                liftEffect $ Console.log $
                  "Description value before submission: '" <> description <> "'"

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
                  liftEffect $ Console.log $ "Description in form submission: '"
                    <> description
                    <> "'"

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
                      liftEffect $ Console.log $
                        "Description after validation: '" <> description <> "'"

                      result <- updateInventory menuItem
                      liftEffect case result of
                        Right (Message msg) -> do
                          Console.info "Submission successful"
                          Console.log $ "Description after submission: '"
                            <> description
                            <> "'"
                          setStatusMessage msg
                        Right (InventoryData _) -> do
                          Console.info "Item updated in inventory"
                          Console.log $ "Description after successful update: '"
                            <> description
                            <> "'"
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
            ( \isSubmitting ->
                if isSubmitting then "Submitting..." else "Submit"
            )
            submittingEvent
        ]

    -- Status message
    , D.div
        [ DA.klass_ "mt-4 text-center" ]
        [ text statusMessageEvent ]

    -- Debug display of current description value
    , D.div
        [ DA.klass_ "mt-4 p-2 bg-gray-100 text-xs" ]
        [ D.h4 [ DA.klass_ "font-bold" ] [ text_ "Debug Info" ]
        , D.div_
            [ text $ descriptionEvent <#> \val -> "Current description state: '"
                <> val
                <> "'"
            ]
        ]
    ]

renderError :: String -> Nut
renderError message =
  D.div
    [ DA.klass_ "error-container max-w-2xl mx-auto p-6" ]
    [ D.div
        [ DA.klass_ "bg-red-100 border-l-4 border-red-500 text-red-700 p-4" ]
        [ D.h2
            [ DA.klass_ "text-lg font-medium mb-2" ]
            [ text_ "Error Loading Item" ]
        , D.p_
            [ text_ message ]
        , D.div
            [ DA.klass_ "mt-4" ]
            [ D.a
                [ DA.href_ "/#/"
                , DA.klass_ "text-blue-600 hover:underline"
                ]
                [ text_ "Return to Inventory" ]
            ]
        ]
    ]