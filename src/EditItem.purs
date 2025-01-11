module EditItem where

import Prelude

import Types (InventoryResponse(..), MenuItem(..), Inventory(..), StrainLineage(..), QueryMode(..))
import UUID (parseUUID)
import Validation (validateMenuItem)
import Form (buttonClass, makeDropdown, makeField, brandConfig, categoryConfig, cbgConfig, 
            creatorConfig, descriptionConfig, dominantTarpeneConfig, effectsConfig, 
            lineageConfig, nameConfig, priceConfig, quantityConfig, skuConfig, 
            speciesConfig, strainConfig, tagsConfig, tarpenesConfig, thcConfig)
import API (updateInventoryInJson, fetchInventory)

import Data.Array (all, find)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String (Pattern(..), split, trim, joinWith)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, useHot)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console

editItem :: String -> Effect Unit
editItem targetUUID = void $ runInBody $ Deku.do
  -- Status and loading state
  setStatusMessage /\ statusMessageEvent <- useState ""
  setSubmitting /\ submittingEvent <- useState false
  setLoading /\ loadingEvent <- useState true

  -- Form field states with hot values for sampling
  setName /\ nameEvent <- useHot ""
  setValidName /\ validNameEvent <- useState (Just false :: Maybe Boolean)
  
  setSku /\ skuEvent <- useHot targetUUID
  setValidSku /\ validSkuEvent <- useState (Just true :: Maybe Boolean)

  setBrand /\ brandEvent <- useHot ""
  setValidBrand /\ validBrandEvent <- useState (Just false :: Maybe Boolean)

  setPrice /\ priceEvent <- useHot ""
  setValidPrice /\ validPriceEvent <- useState (Just false :: Maybe Boolean)

  setQuantity /\ quantityEvent <- useHot ""
  setValidQuantity /\ validQuantityEvent <- useState (Just false :: Maybe Boolean)

  setCategory /\ categoryEvent <- useHot ""
  setValidCategory /\ validCategoryEvent <- useState (Just false :: Maybe Boolean)

  setDescription /\ descriptionEvent <- useHot ""
  setTags /\ tagsEvent <- useHot ""
  setEffects /\ effectsEvent <- useHot ""
  setTarpenes /\ tarpenesEvent <- useHot ""
  setLineage /\ lineageEvent <- useHot ""

  setThc /\ thcEvent <- useHot ""
  setValidThc /\ validThcEvent <- useState (Just false :: Maybe Boolean)

  setCbg /\ cbgEvent <- useHot ""
  setValidCbg /\ validCbgEvent <- useState (Just false :: Maybe Boolean)

  setStrain /\ strainEvent <- useHot ""
  setValidStrain /\ validStrainEvent <- useState (Just false :: Maybe Boolean)

  setCreator /\ creatorEvent <- useHot ""
  setValidCreator /\ validCreatorEvent <- useState (Just false :: Maybe Boolean)

  setSpecies /\ speciesEvent <- useHot ""
  setValidSpecies /\ validSpeciesEvent <- useState (Just false :: Maybe Boolean)

  setDominantTarpene /\ dominantTarpeneEvent <- useHot ""
  setValidDominantTarpene /\ validDominantTarpeneEvent <- useState (Just false :: Maybe Boolean)

  -- Load initial data
  void $ Aff.launchAff_ do
    result <- fetchInventory JsonMode
    liftEffect $ case result of
      Right (InventoryData inv) -> 
        case find (\(MenuItem item) -> show item.sku == targetUUID) inv of
          Just (MenuItem item) -> do
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
            setCategory (show item.category)
            setValidCategory (Just true)
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
            setLoading false
          Nothing -> do
            setStatusMessage "Item not found"
            setLoading false
      Left err -> do
        setStatusMessage $ "Error loading item: " <> err
        setLoading false
      _ -> do
        setStatusMessage "Unexpected response"
        setLoading false

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
      vDominantTarpene <- validDominantTarpeneEvent
      in all (fromMaybe false)
        [ vName, vSku, vBrand, vPrice, vQuantity, vCategory
        , vThc, vCbg, vStrain, vCreator, vSpecies
        , vDominantTarpene
        ]

    ensureNumber :: String -> String
    ensureNumber str = fromMaybe "0.0" $ map show $ Number.fromString $ trim str

    ensureInt :: String -> String
    ensureInt str = fromMaybe "0" $ map show $ fromString $ trim str

  D.div_
    [ D.div
        [ DA.klass_ "space-y-4 max-w-2xl mx-auto p-6" ]
        [ D.h2 
            [ DA.klass_ "text-2xl font-bold mb-6" ]
            [ text_ "Edit Menu Item" ]
        , D.div 
            [ DA.klass_ "mb-4" ]
            [ text $ map (\loading -> 
                if loading then "Loading..." else ""
              ) loadingEvent 
            ]
        , makeField (nameConfig "") setName setValidName validNameEvent
        , makeField (skuConfig targetUUID) setSku setValidSku validSkuEvent
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
        , DL.click_ \_ -> do
            setSubmitting true
            Aff.launchAff_ do
              let formInput = 
                    { name: nameEvent.value
                    , sku: skuEvent.value
                    , brand: brandEvent.value
                    , price: ensureNumber priceEvent.value
                    , quantity: ensureInt quantityEvent.value
                    , category: categoryEvent.value
                    , description: descriptionEvent.value
                    , tags: split (Pattern ",") tagsEvent.value
                    , effects: split (Pattern ",") effectsEvent.value
                    , strainLineage:
                        { thc: thcEvent.value
                        , cbg: cbgEvent.value
                        , strain: strainEvent.value
                        , creator: creatorEvent.value
                        , species: speciesEvent.value
                        , dominant_tarpene: dominantTarpeneEvent.value
                        , tarpenes: split (Pattern ",") tarpenesEvent.value
                        , lineage: split (Pattern ",") lineageEvent.value
                        }
                    }
              
              case validateMenuItem formInput of
                Left err -> liftEffect do
                  setStatusMessage $ "Validation error: " <> err
                  setSubmitting false
                
                Right menuItem -> do
                  result <- updateInventoryInJson menuItem
                  liftEffect case result of
                    Right (Message msg) -> do
                      Console.info "Update successful"
                      setStatusMessage msg
                      setSubmitting false
                    Right (InventoryData _) -> do
                      Console.info "Item updated in inventory"
                      setStatusMessage "Item successfully updated!"
                      setSubmitting false
                    Left err -> do
                      Console.error "File System Error:"
                      Console.errorShow err
                      setStatusMessage $ "Error updating item: " <> err
                      setSubmitting false
        ]
        [ text $ map (\submitting -> 
            if submitting then "Updating..." else "Update"
          ) submittingEvent 
        ]
    , D.div
        [ DA.klass_ "mt-4 text-center" ]
        [ text statusMessageEvent ]
    ]