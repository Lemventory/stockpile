module EditItem where

import Prelude

import API (updateInventoryInJson, fetchInventory)
import Data.Array (all, find)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff, launchAff_)
import Effect.Class (liftEffect)
import Form (buttonClass, makeDropdown, makeField, brandConfig, categoryConfig, cbgConfig, creatorConfig, descriptionConfig, dominantTarpeneConfig, effectsConfig, lineageConfig, nameConfig, priceConfig, quantityConfig, skuConfig, speciesConfig, strainConfig, tagsConfig, tarpenesConfig, thcConfig)
import Types (InventoryResponse(..), MenuItem(..), Inventory(..), StrainLineage(..), QueryMode(..))
import Validation (validateMenuItem)

editItem :: String -> Effect Unit
editItem targetUUID = void $ runInBody Deku.do
  -- Status and loading state
  setStatusMessage /\ statusMessageEvent <- useState ""
  setSubmitting /\ submittingEvent <- useState false
  setLoading /\ loadingEvent <- useState true
  setFiber /\ _ <- useState (pure unit)

  -- Form field states
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

  setCategory /\ categoryEvent <- useState ""
  setValidCategory /\ validCategoryEvent <- useState (Just false :: Maybe Boolean)

  -- Optional fields
  setDescription /\ descriptionEvent <- useState ""
  setTags /\ tagsEvent <- useState ""
  setEffects /\ effectsEvent <- useState ""
  setTarpenes /\ tarpenesEvent <- useState ""
  setLineage /\ lineageEvent <- useState ""

  -- StrainLineage fields
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

  D.div [] [ D.div_
    [ D.button
        [ DA.klass_ $ buttonClass "blue"
        , DL.click_ \_ -> launchAff_ do
            result <- fetchInventory JsonMode
            liftEffect $ case result of
              Right (InventoryData (Inventory items)) -> 
                case find (\(MenuItem item) -> show item.sku == targetUUID) items of
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
        ]
        [ text_ "Load Item" ]
    , D.div
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
        , DL.runOn DL.click $ 
            (\name sku brand price quantity category description tags effects 
              thc cbg strain creator species dominant_tarpene tarpenes lineage -> do
                setSubmitting true
                void $ setFiber =<< launchAff do
                  let formInput = 
                        { name
                        , sku
                        , brand
                        , price
                        , quantity
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
                  
                  case validateMenuItem formInput of
                    Left err -> liftEffect do
                      setStatusMessage $ "Validation error: " <> err
                      setSubmitting false
                    
                    Right menuItem -> do
                      result <- updateInventoryInJson menuItem
                      liftEffect case result of
                        Right (Message msg) -> do
                          setStatusMessage msg
                          setSubmitting false
                        Right (InventoryData _) -> do
                          setStatusMessage "Item successfully updated!"
                          setSubmitting false
                        Left err -> do
                          setStatusMessage $ "Error updating item: " <> err
                          setSubmitting false
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
            if submitting then "Updating..." else "Update"
          ) submittingEvent 
        ]
    , D.div
        [ DA.klass_ "mt-4 text-center" ]
        [ text statusMessageEvent ]
    ] ]