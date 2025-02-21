{-
Generated: 2025-02-21 12:15:05
Hash: 848ead2ef77073f69900f78f66488cd5bcec4b613d73078b056f5048a0419d08
-}

{-
COMPILE_STATUS: false
COMPILE_ERROR:
Reading Spago workspace configuration...

✓ Selecting package to build: cheeblr

Downloading dependencies...
Building...
[1 of 2] Compiling EditItem
[WARNING 1/1 UnusedName] src/EditItem.purs:178:34

  178    useHot Beginning \(setState /\ event) ->
                                        ^^^^^

  Name event was introduced but not used.
  in value declaration editItem

[ERROR 1/1 TypesDoNotUnify] src/EditItem.purs:181:13

                   v
  181            [ DL.runOn DL.load \_ -> do
  182                setState Loading
  183                launchAff_ do
       ...
  193                        Right (Message msg) -> liftEffect $ setState $ Error msg
  194                        Left err -> liftEffect $ setState $ Error err
  195                      Left err -> liftEffect $ setState $ Error $ show err
                                                                              ^

  Could not match type
    Function t2
  with type
    Poll
  while trying to match type t0
                               (Attribute
                                  ( load :: Event
                                  | t1
                                  )
                               )
    with type Poll
                (Attribute
                   ( __tag :: ... "HTMLSpanElement"
                   , __tag :: ... "HTMLElement"
                   , __tag :: ... "HTMLOrSVGElement"
                   , __tag :: ... "ElementContentEditable"
                   , __tag :: ... "GlobalEventHandlers"
                   , __tag :: ... "Element"
                   , __tag :: ... "ARIAMixin"
                   , __tag :: ... "Slottable"
                   , __tag :: ... "ChildNode"
                   , __tag :: ... "NonDocumentTypeChildNode"
                   , __tag :: ... "ParentNode"
                   , __tag :: ... "Node"
                   , __tag :: ... "EventTarget"
                   , abort :: Event
                   , accesskey :: String
                   , animationcancel :: Event
                   , animationend :: Event
                   , animationiteration :: Event
                   , animationstart :: Event
                   , ariaActivedescendant :: String
                   , ariaAtomic :: String
                   , ariaAutocomplete :: String
                   , ariaBraillelabel :: String
                   , ariaBrailleroledescription :: String
                   , ariaBusy :: String
                   , ariaChecked :: String
                   , ariaColcount :: String
                   , ariaColindex :: String
                   , ariaColindextext :: String
                   , ariaColspan :: String
                   , ariaControls :: String
                   , ariaCurrent :: String
                   , ariaDescribedby :: String
                   , ariaDescription :: String
                   , ariaDetails :: String
                   , ariaDisabled :: String
                   , ariaErrormessage :: String
                   , ariaExpanded :: String
                   , ariaFlowto :: String
                   , ariaHaspopup :: String
                   , ariaHidden :: String
                   , ariaInvalid :: String
                   , ariaKeyshortcuts :: String
                   , ariaLabel :: String
                   , ariaLabelledby :: String
                   , ariaLevel :: String
                   , ariaLive :: String
                   , ariaModal :: String
                   , ariaMultiline :: String
                   , ariaMultiselectable :: String
                   , ariaOrientation :: String
                   , ariaOwns :: String
                   , ariaPlaceholder :: String
                   , ariaPosinset :: String
                   , ariaPressed :: String
                   , ariaReadonly :: String
                   , ariaRequired :: String
                   , ariaRoledescription :: String
                   , ariaRowcount :: String
                   , ariaRowindex :: String
                   , ariaRowindextext :: String
                   , ariaRowspan :: String
                   , ariaSelected :: String
                   , ariaSetsize :: String
                   , ariaSort :: String
                   , ariaValuemax :: String
                   , ariaValuemin :: String
                   , ariaValuenow :: String
                   , ariaValuetext :: String
                   , autocapitalize :: String
                   , autofocus :: String
                   , auxclick :: PointerEvent
                   , beforeinput :: Event
                   , beforematch :: Event
                   , beforetoggle :: Event
                   , blur :: FocusEvent
                   , click :: PointerEvent
                   , compositionend :: CompositionEvent
                   , compositionstart :: CompositionEvent
                   , compositionupdate :: CompositionEvent
                   , contenteditable :: String
                   , contextmenu :: PointerEvent
                   , dblclick :: MouseEvent
                   , dir :: String
                   , domActivate :: UIEvent
                   , domAttrModified :: Event
                   , domFocusIn :: FocusEvent
                   , domFocusOut :: FocusEvent
                   , domNodeInserted :: Event
                   , domNodeInsertedIntoDocument :: Event
                   , domNodeRemoved :: Event
                   , domNodeRemovedFromDocument :: Event
                   , domSubtreeModified :: Event
                   , drag :: DragEvent
                   , dragend :: DragEvent
                   , dragenter :: DragEvent
                   , draggable :: String
                   , dragleave :: DragEvent
                   , dragover :: DragEvent
                   , dragstart :: DragEvent
                   , drop :: DragEvent
                   , enterkeyhint :: String
                   , error :: Event
                   , error :: Event
                   , focus :: FocusEvent
                   , focusin :: FocusEvent
                   , focusout :: FocusEvent
                   , gotpointercapture :: PointerEvent
                   , hidden :: String
                   , id :: String
                   , input :: Event
                   , inputmode :: String
                   , is :: String
                   , itemid :: String
                   , itemprop :: String
                   , itemref :: String
                   , itemscope :: String
                   , itemtype :: String
                   , keydown :: KeyboardEvent
                   , keypress :: KeyboardEvent
                   , keyup :: KeyboardEvent
                   , klass :: String
                   , lang :: String
                   , load :: Event
                   , load :: Event
                   , lostpointercapture :: PointerEvent
                   , mousedown :: MouseEvent
                   , mouseenter :: MouseEvent
                   , mouseleave :: MouseEvent
                   , mousemove :: MouseEvent
                   , mouseout :: MouseEvent
                   , mouseover :: MouseEvent
                   , mouseup :: MouseEvent
                   , nonce :: String
                   , pointercancel :: PointerEvent
                   , pointerdown :: PointerEvent
                   , pointerenter :: PointerEvent
                   , pointerleave :: PointerEvent
                   , pointermove :: PointerEvent
                   , pointerout :: PointerEvent
                   , pointerover :: PointerEvent
                   , pointerrawupdate :: PointerEvent
                   , pointerup :: PointerEvent
                   , popover :: String
                   , popovertarget :: String
                   , popovertargetaction :: String
                   , role :: String
                   , select :: Event
                   , slot :: String
                   , spellcheck :: String
                   , style :: String
                   , tabindex :: String
                   , textInput :: Event
                   , title :: String
                   , touchcancel :: TouchEvent
                   , touchend :: TouchEvent
                   , touchmove :: TouchEvent
                   , touchstart :: TouchEvent
                   , transitioncancel :: Event
                   , transitionend :: Event
                   , transitionrun :: Event
                   , transitionstart :: Event
                   , translate :: String
                   , unload :: Event
                   , wheel :: Event
                   , writingsuggestions :: String
                   )
                )
  while checking that expression (runOn load) (\$63 ->
                                                 case $63 of
                                                   _ -> ...
                                              )
    has type Poll
               (Attribute
                  ( __tag :: ... "HTMLSpanElement"
                  , __tag :: ... "HTMLElement"
                  , __tag :: ... "HTMLOrSVGElement"
                  , __tag :: ... "ElementContentEditable"
                  , __tag :: ... "GlobalEventHandlers"
                  , __tag :: ... "Element"
                  , __tag :: ... "ARIAMixin"
                  , __tag :: ... "Slottable"
                  , __tag :: ... "ChildNode"
                  , __tag :: ... "NonDocumentTypeChildNode"
                  , __tag :: ... "ParentNode"
                  , __tag :: ... "Node"
                  , __tag :: ... "EventTarget"
                  , abort :: Event
                  , accesskey :: String
                  , animationcancel :: Event
                  , animationend :: Event
                  , animationiteration :: Event
                  , animationstart :: Event
                  , ariaActivedescendant :: String
                  , ariaAtomic :: String
                  , ariaAutocomplete :: String
                  , ariaBraillelabel :: String
                  , ariaBrailleroledescription :: String
                  , ariaBusy :: String
                  , ariaChecked :: String
                  , ariaColcount :: String
                  , ariaColindex :: String
                  , ariaColindextext :: String
                  , ariaColspan :: String
                  , ariaControls :: String
                  , ariaCurrent :: String
                  , ariaDescribedby :: String
                  , ariaDescription :: String
                  , ariaDetails :: String
                  , ariaDisabled :: String
                  , ariaErrormessage :: String
                  , ariaExpanded :: String
                  , ariaFlowto :: String
                  , ariaHaspopup :: String
                  , ariaHidden :: String
                  , ariaInvalid :: String
                  , ariaKeyshortcuts :: String
                  , ariaLabel :: String
                  , ariaLabelledby :: String
                  , ariaLevel :: String
                  , ariaLive :: String
                  , ariaModal :: String
                  , ariaMultiline :: String
                  , ariaMultiselectable :: String
                  , ariaOrientation :: String
                  , ariaOwns :: String
                  , ariaPlaceholder :: String
                  , ariaPosinset :: String
                  , ariaPressed :: String
                  , ariaReadonly :: String
                  , ariaRequired :: String
                  , ariaRoledescription :: String
                  , ariaRowcount :: String
                  , ariaRowindex :: String
                  , ariaRowindextext :: String
                  , ariaRowspan :: String
                  , ariaSelected :: String
                  , ariaSetsize :: String
                  , ariaSort :: String
                  , ariaValuemax :: String
                  , ariaValuemin :: String
                  , ariaValuenow :: String
                  , ariaValuetext :: String
                  , autocapitalize :: String
                  , autofocus :: String
                  , auxclick :: PointerEvent
                  , beforeinput :: Event
                  , beforematch :: Event
                  , beforetoggle :: Event
                  , blur :: FocusEvent
                  , click :: PointerEvent
                  , compositionend :: CompositionEvent
                  , compositionstart :: CompositionEvent
                  , compositionupdate :: CompositionEvent
                  , contenteditable :: String
                  , contextmenu :: PointerEvent
                  , dblclick :: MouseEvent
                  , dir :: String
                  , domActivate :: UIEvent
                  , domAttrModified :: Event
                  , domFocusIn :: FocusEvent
                  , domFocusOut :: FocusEvent
                  , domNodeInserted :: Event
                  , domNodeInsertedIntoDocument :: Event
                  , domNodeRemoved :: Event
                  , domNodeRemovedFromDocument :: Event
                  , domSubtreeModified :: Event
                  , drag :: DragEvent
                  , dragend :: DragEvent
                  , dragenter :: DragEvent
                  , draggable :: String
                  , dragleave :: DragEvent
                  , dragover :: DragEvent
                  , dragstart :: DragEvent
                  , drop :: DragEvent
                  , enterkeyhint :: String
                  , error :: Event
                  , error :: Event
                  , focus :: FocusEvent
                  , focusin :: FocusEvent
                  , focusout :: FocusEvent
                  , gotpointercapture :: PointerEvent
                  , hidden :: String
                  , id :: String
                  , input :: Event
                  , inputmode :: String
                  , is :: String
                  , itemid :: String
                  , itemprop :: String
                  , itemref :: String
                  , itemscope :: String
                  , itemtype :: String
                  , keydown :: KeyboardEvent
                  , keypress :: KeyboardEvent
                  , keyup :: KeyboardEvent
                  , klass :: String
                  , lang :: String
                  , load :: Event
                  , load :: Event
                  , lostpointercapture :: PointerEvent
                  , mousedown :: MouseEvent
                  , mouseenter :: MouseEvent
                  , mouseleave :: MouseEvent
                  , mousemove :: MouseEvent
                  , mouseout :: MouseEvent
                  , mouseover :: MouseEvent
                  , mouseup :: MouseEvent
                  , nonce :: String
                  , pointercancel :: PointerEvent
                  , pointerdown :: PointerEvent
                  , pointerenter :: PointerEvent
                  , pointerleave :: PointerEvent
                  , pointermove :: PointerEvent
                  , pointerout :: PointerEvent
                  , pointerover :: PointerEvent
                  , pointerrawupdate :: PointerEvent
                  , pointerup :: PointerEvent
                  , popover :: String
                  , popovertarget :: String
                  , popovertargetaction :: String
                  , role :: String
                  , select :: Event
                  , slot :: String
                  , spellcheck :: String
                  , style :: String
                  , tabindex :: String
                  , textInput :: Event
                  , title :: String
                  , touchcancel :: TouchEvent
                  , touchend :: TouchEvent
                  , touchmove :: TouchEvent
                  , touchstart :: TouchEvent
                  , transitioncancel :: Event
                  , transitionend :: Event
                  , transitionrun :: Event
                  , transitionstart :: Event
                  , translate :: String
                  , unload :: Event
                  , wheel :: Event
                  , writingsuggestions :: String
                  )
               )
  in value declaration editItem
  where t1 is an unknown type
        t0 is an unknown type
        t2 is an unknown type

           Src   Lib   All
Warnings     1     0     1
Errors       1     0     1

✘ Failed to build.

-}

-- FILE: frontend/src/EditItem.purs
module EditItem where

import Prelude

import API (readInventory, updateInventory)
import Data.Array (all, find)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.List (Pattern(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String (joinWith, split, trim)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (fixed, useHot)
import Deku.Core as Core
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useState, (<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (error, joinFiber, killFiber, launchAff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
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
  setSubmitting /\ submittingEvent <- useState false

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

  setDominantTarpene /\ dominantTarpeneEvent <- useState ""
  setValidDominantTarpene /\ validDominantTarpeneEvent <- useState (Just false)

  setTarpenes /\ tarpenesEvent <- useState ""
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
      vDominantTarpene <- validDominantTarpeneEvent
      vLeaflyUrl <- validLeaflyUrlEvent
      vImg <- validImgEvent
      in all (fromMaybe false)
        [ vName, vSku, vBrand, vPrice, vQuantity, vSort
        , vMeasureUnit, vPerPackage, vCategory, vSubcategory
        , vThc, vCbg, vStrain, vCreator, vSpecies
        , vDominantTarpene, vLeaflyUrl, vImg
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

  useHot Beginning \(setState /\ event) ->
    D.div_
      [ D.span
          [ DL.runOn DL.load \_ -> do
              setState Loading
              launchAff_ do
                  result <- try readInventory
                  case result of
                    Right res -> case res of
                      Right (InventoryData (Inventory items)) ->
                        liftEffect case find (\(MenuItem item) -> show item.sku == targetUUID) items of
                          Just menuItem -> do
                            updateFormFields menuItem
                            setState $ Loaded menuItem
                          Nothing -> setState Beginning
                      Right (Message msg) -> liftEffect $ setState $ Error msg
                      Left err -> liftEffect $ setState $ Error err
                    Left err -> liftEffect $ setState $ Error $ show err
            ]
            []
        ]

  D.div
    [ DA.klass_ "space-y-4 max-w-2xl mx-auto p-6" ]
    [ D.div_
        [ uiState <#~> case _ of
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
              , makeField (dominantTarpeneConfig "") setDominantTarpene setValidDominantTarpene validDominantTarpeneEvent
              , makeField (tarpenesConfig "") setTarpenes (const $ pure unit) (pure $ Just true)
              , makeField (lineageConfig "") setLineage (const $ pure unit) (pure $ Just true)
              , makeField (leaflyUrlConfig "") setLeaflyUrl setValidLeaflyUrl validLeaflyUrlEvent
              , makeField (imgConfig "") setImg setValidImg validImgEvent

              , D.button
                  [ DA.klass_ $ buttonClass "green"
                  , DA.disabled $ map show $ (||) <$> submittingEvent <*> map not isFormValid
                  , DL.click_ \_ -> do
                      sort <- sortEvent
                      name <- nameEvent
                      sku <- skuEvent
                      brand <- brandEvent
                      price <- priceEvent
                      measureUnit <- measureUnitEvent
                      perPackage <- perPackageEvent
                      quantity <- quantityEvent
                      category <- categoryEvent
                      subcategory <- subcategoryEvent
                      description <- descriptionEvent
                      tags <- tagsEvent
                      effects <- effectsEvent
                      thc <- thcEvent
                      cbg <- cbgEvent
                      strain <- strainEvent
                      creator <- creatorEvent
                      species <- speciesEvent
                      dominantTarpene <- dominantTarpeneEvent
                      tarpenes <- tarpenesEvent
                      lineage <- lineageEvent
                      leaflyUrl <- leaflyUrlEvent
                      img <- imgEvent
                      submitting <- submittingEvent

                      when (not submitting) do
                        setSubmitting true
                        launchAff_ do
                          let formInput =
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
                                , tags: split (Pattern ",") >>> map trim $ tags
                                , effects: split (Pattern ",") >>> map trim $ effects
                                , strain_lineage:
                                    { thc
                                    , cbg
                                    , strain
                                    , creator
                                    , species
                                    , dominant_tarpene: dominantTarpene
                                    , tarpenes: split (Pattern ",") >>> map trim $ tarpenes
                                    , lineage: split (Pattern ",") >>> map trim $ lineage
                                    , leafly_url: leaflyUrl
                                    , img
                                    }
                                }

                          case validateMenuItem formInput of
                            Left err -> liftEffect do
                              Console.error "Form validation failed:"
                              Console.errorShow err
                              setUIState $ Error $ "Validation error: " <> err
                              setSubmitting false

                            Right menuItem -> do
                              result <- try $ updateInventory menuItem
                              case result of
                                Right (Right (Message msg)) -> liftEffect do
                                  Console.info "Update successful"
                                  setUIState $ Error msg
                                Right (Right (InventoryData _)) -> liftEffect do
                                  Console.info "Item updated in inventory"
                                  setUIState $ Error "Item successfully updated!"
                                Right (Left err) -> liftEffect do
                                  Console.error "API Error:"
                                  Console.errorShow err
                                  setUIState $ Error err
                                Left err -> liftEffect do
                                  Console.error "API Error:"
                                  Console.errorShow err
                                  setUIState $ Error $ show err
                              liftEffect $ setSubmitting false
                  ]
                  [ text $ map
                      (\submitting -> if submitting then "Updating..." else "Update")
                      submittingEvent
                  ]
              ]
        ]
    ]-- END FILE

-- FILE: frontend/src/API.purs
module API where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Fetch (Method(..), fetch)
import Fetch.Yoga.Json (fromJSON)
import Types (Inventory, InventoryResponse(..), MenuItem)
import Yoga.JSON (writeJSON)

baseUrl :: String
baseUrl = "http://localhost:8080"

writeInventory :: MenuItem -> Aff (Either String InventoryResponse)
writeInventory menuItem = do
  result <- attempt do
    let content = writeJSON menuItem

    liftEffect $ Console.log "Creating new menu item..."
    liftEffect $ Console.log $ "Sending content: " <> content

    response <- fetch (baseUrl <> "/inventory")
      { method: POST
      , body: content
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": "http://localhost:5174"
          }
      }

    fromJSON response.json

  pure case result of
    Left err -> Left $ "Create error: " <> show err
    Right response -> Right response

readInventory :: Aff (Either String InventoryResponse)
readInventory = do
  result <- attempt do
    response <- fetch (baseUrl <> "/inventory")
      { method: GET
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": "http://localhost:5174"
          }
      }


    fromJSON response.json :: Aff InventoryResponse

  pure case result of
    Left err -> Left $ "Failed to read inventory: " <> show err
    Right response -> Right response

updateInventory :: MenuItem -> Aff (Either String InventoryResponse)
updateInventory menuItem = do
  result <- attempt do
    let content = writeJSON menuItem

    liftEffect $ Console.log "Updating menu item..."

    response <- fetch (baseUrl <> "/inventory")
      { method: PUT
      , body: content
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          }
      }

    res <- fromJSON response.json :: Aff InventoryResponse
    pure res

  pure case result of
    Left err -> Left $ "Update error: " <> show err
    Right response -> Right response

deleteInventory :: String -> Aff (Either String InventoryResponse)
deleteInventory itemId = do
  result <- attempt do
    response <- fetch (baseUrl <> "/inventory/" <> itemId)
      { method: DELETE
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          }
      }
    res <- fromJSON response.json :: Aff InventoryResponse
    pure res

  pure case result of
    Left err -> Left $ "Delete error: " <> show err
    Right response -> Right response-- END FILE

-- FILE: frontend/src/CreateItem.purs
module CreateItem where

import Prelude

import API (writeInventory)
import Data.Array (all)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
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
import Form (brandConfig, buttonClass, categoryConfig, cbgConfig, creatorConfig, descriptionConfig, dominantTarpeneConfig, effectsConfig, imgConfig, leaflyUrlConfig, lineageConfig, makeDropdown, makeField, measureUnitConfig, nameConfig, perPackageConfig, priceConfig, quantityConfig, skuConfig, sortConfig, speciesConfig, strainConfig, subcategoryConfig, tagsConfig, tarpenesConfig, thcConfig)
import Types (InventoryResponse(..))
import UUID (genUUID)
import Utils (ensureInt, ensureNumber)
import Validation (validateMenuItem)

createItem :: Effect Unit
createItem = do
  initialId <- genUUID
  let initialSkuStr = show initialId

  void $ runInBody Deku.do

    setStatusMessage /\ statusMessageEvent <- useState ""
    setSubmitting /\ submittingEvent <- useState false
    setFiber /\ _ <- useState (pure unit)


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


    setSort /\ sortEvent <- useState ""
    setValidSort /\ validSortEvent <- useState (Just false :: Maybe Boolean)

    setMeasureUnit /\ measureUnitEvent <- useState ""
    setValidMeasureUnit /\ validMeasureUnitEvent <- useState (Just false :: Maybe Boolean)

    setPerPackage /\ perPackageEvent <- useState ""
    setValidPerPackage /\ validPerPackageEvent <- useState (Just false :: Maybe Boolean)

    setSubcategory /\ subcategoryEvent <- useState ""
    setValidSubcategory /\ validSubcategoryEvent <- useState (Just false :: Maybe Boolean)


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
        vCategory <- validCategoryEvent
        vThc <- validThcEvent
        vCbg <- validCbgEvent
        vStrain <- validStrainEvent
        vCreator <- validCreatorEvent
        vSpecies <- validSpeciesEvent
        vDominantTarpene <- validDominantTarpeneEvent
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
      ]-- END FILE

-- FILE: frontend/src/DataFetcher.purs
module MenuLiveView.DataFetcher where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Now (now)
import Fetch (Method(..), fetch)
import Fetch.Yoga.Json (fromJSON)
import Types (Inventory, InventoryResponse(..))

data QueryMode = JsonMode | HttpMode

derive instance eqQueryMode :: Eq QueryMode
derive instance ordQueryMode :: Ord QueryMode

instance Show QueryMode where
  show JsonMode = "JsonMode"
  show HttpMode = "HttpMode"

type FetchConfig =
  { apiEndpoint :: String
  , jsonPath :: String
  , corsHeaders :: Boolean
  }

defaultConfig :: FetchConfig
defaultConfig =
  { apiEndpoint: "http://localhost:8080/inventory"
  , jsonPath: "./inventory.json"
  , corsHeaders: true
  }

fetchInventory :: FetchConfig -> QueryMode -> Aff (Either String InventoryResponse)
fetchInventory config = case _ of
  JsonMode -> do
    liftEffect $ Console.log "Using JSON mode (local file)"
    fetchInventoryFromJson config
  HttpMode -> do
    liftEffect $ Console.log "Using HTTP mode (backend API)"
    fetchInventoryFromHttp config

fetchInventoryFromJson :: FetchConfig -> Aff (Either String InventoryResponse)
fetchInventoryFromJson config = do
  result <- attempt do
    timestamp <- liftEffect $ show <$> now
    let url = config.jsonPath <> "?t=" <> timestamp
    liftEffect $ Console.log ("Fetching from JSON: " <> url)

    response <- fetch url {}
    inventory <- fromJSON response.json :: Aff Inventory
    pure inventory

  pure case result of
    Left err -> Left $ "JSON fetch error: " <> show err
    Right inventory -> Right $ InventoryData inventory

fetchInventoryFromHttp :: FetchConfig -> Aff (Either String InventoryResponse)
fetchInventoryFromHttp config = do
  result <- attempt do
    liftEffect $ Console.log ("Fetching from API: " <> config.apiEndpoint)

    response <- fetch config.apiEndpoint
      { method: GET
      , headers:
          { "Content-Type": "application/json"
          , "Accept": "application/json"
          , "Origin": "http://localhost:5174"
          }
      }

    inventoryResponse <- fromJSON response.json :: Aff InventoryResponse
    liftEffect $ Console.log "Got response from server"
    pure inventoryResponse

  case result of
    Left err -> do
      liftEffect $ Console.error $ "API fetch error details: " <> show err
      pure $ Left $ "API fetch error: " <> show err
    Right response -> pure $ Right response-- END FILE

-- FILE: frontend/src/Form.purs
module Form where

import Prelude

import Data.Array ((:))
import Data.Enum (class BoundedEnum)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), trim, replaceAll)
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Effect (Effect)
import FRP.Poll (Poll)
import Types (DropdownConfig, FieldConfig, ItemCategory, Species, ValidationPreset, runValidation)
import Utils (getAllEnumValues, parseCommaList)
import Validation (alphanumeric, allOf, commaListField, moneyField, multilineText, nonEmpty, numberField, percentageField, requiredText, requiredTextWithLimit, validUUID)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value) as Input
import Web.HTML.HTMLSelectElement (fromEventTarget, value) as Select
import Web.UIEvent.KeyboardEvent (toEvent)

makeField :: FieldConfig -> (String -> Effect Unit) -> (Maybe Boolean -> Effect Unit) -> Poll (Maybe Boolean) -> Nut
makeField config setValue setValid validEvent =
  D.div_
    [ D.div
        [ DA.klass_ "flex items-center gap-2" ]
        [ D.label_
            [ text_ config.label ]
        , if config.label == "Description" then D.textarea
            [ DA.placeholder_ config.placeholder
            , DA.cols_ "40"
            , DA.rows_ "4"
            , DL.keyup_ \evt -> do
                let targetEvent = toEvent evt
                for_
                  (target targetEvent >>= Input.fromEventTarget)
                  \inputElement -> do
                    v <- Input.value inputElement
                    let formatted = config.formatInput v
                    setValue formatted
                    setValid (Just (runValidation config.validation formatted))
            , DL.input_ \evt -> do
                for_
                  (target evt >>= Input.fromEventTarget)
                  \inputElement -> do
                    v <- Input.value inputElement
                    let formatted = config.formatInput v
                    setValue formatted
                    setValid (Just (runValidation config.validation formatted))
            , DA.klass_ (inputKls <> " resize-y")
            ]
            [ text_ config.defaultValue ]
          else D.input
            [ DA.placeholder_ config.placeholder
            , DA.value_ config.defaultValue
            , DL.keyup_ \evt -> do
                let targetEvent = toEvent evt
                for_
                  (target targetEvent >>= Input.fromEventTarget)
                  \inputElement -> do
                    v <- Input.value inputElement
                    let formatted = config.formatInput v
                    setValue formatted
                    setValid (Just (runValidation config.validation formatted))
            , DL.input_ \evt -> do
                for_
                  (target evt >>= Input.fromEventTarget)
                  \inputElement -> do
                    v <- Input.value inputElement
                    let formatted = config.formatInput v
                    setValue formatted
                    setValid (Just (runValidation config.validation formatted))
            , DA.klass_ inputKls
            ]
            []
        , D.span
            [ DA.klass_ "text-red-500 text-xs" ]
            [ text
                ( map
                    ( \mValid -> case mValid of
                        Just false -> config.errorMessage
                        _ -> ""
                    )
                    validEvent
                )
            ]
        ]
    ]

makeDropdown :: DropdownConfig -> (String -> Effect Unit) -> (Maybe Boolean -> Effect Unit) -> Poll (Maybe Boolean) -> Nut
makeDropdown config setValue setValid validEvent =
  D.div_
    [ D.div
        [ DA.klass_ "flex items-center gap-2" ]
        [ D.label_
            [ text_ config.label ]
        , D.select
            [ DA.klass_ inputKls
            , DL.change_ \evt -> do
                for_
                  (target evt >>= Select.fromEventTarget)
                  \selectElement -> do
                    v <- Select.value selectElement
                    setValue v
                    setValid (Just (v /= ""))
            ]
            ( config.options <#> \opt ->
                D.option
                  [ DA.value_ opt.value ]
                  [ text_ opt.label ]
            )
        , D.span
            [ DA.klass_ "text-red-500 text-xs" ]
            [ text
                ( map
                    ( \mValid -> case mValid of
                        Just false -> "Please select an option"
                        _ -> ""
                    )
                    validEvent
                )
            ]
        ]
    ]

makeEnumDropdown
  :: ∀ a
   . BoundedEnum a
  => Bounded a
  => Show a
  => { label :: String, enumType :: a }
  -> DropdownConfig
makeEnumDropdown { label } =
  { label
  , options:
      { value: "", label: "Select..." } :
        map (\val -> { value: show val, label: show val })
          (getAllEnumValues :: Array a)
  , defaultValue: ""
  }

makeArrayField :: String -> (Array String -> Effect Unit) -> Nut
makeArrayField label setValue =
  D.div_
    [ D.div
        [ DA.klass_ "flex items-center gap-2" ]
        [ D.label_
            [ text_ label ]
        , D.input
            [ DA.placeholder_ "Add items (comma-separated)"
            , DL.keyup_ \evt -> do
                for_
                  ((target >=> Input.fromEventTarget) (toEvent evt))
                  \inputElement -> do
                    v <- Input.value inputElement
                    setValue $ parseCommaList v
            , DA.klass_ inputKls
            ]
            []
        ]
    ]

makeFieldConfig :: String -> String -> String -> ValidationPreset -> FieldConfig
makeFieldConfig label placeholder defaultValue preset =
  { label
  , placeholder
  , defaultValue
  , validation: preset.validation
  , errorMessage: preset.errorMessage
  , formatInput: preset.formatInput
  }

categoryConfig :: DropdownConfig
categoryConfig = makeEnumDropdown
  { label: "Category"
  , enumType: (bottom :: ItemCategory)
  }

speciesConfig :: DropdownConfig
speciesConfig = makeEnumDropdown
  { label: "Species"
  , enumType: (bottom :: Species)
  }

skuConfig :: String -> FieldConfig
skuConfig defaultValue = makeFieldConfig "SKU" "Enter UUID" defaultValue
  { validation: allOf [ nonEmpty, validUUID ]
  , errorMessage: "Required, must be a valid UUID"
  , formatInput: trim
  }

nameConfig :: String -> FieldConfig
nameConfig defaultValue = makeFieldConfig "Name" "Enter product name" defaultValue
  (requiredTextWithLimit 50)

brandConfig :: String -> FieldConfig
brandConfig defaultValue = makeFieldConfig "Brand" "Enter brand name" defaultValue
  (requiredTextWithLimit 30)

priceConfig :: String -> FieldConfig
priceConfig defaultValue = makeFieldConfig "Price" "Enter price" defaultValue
  moneyField

quantityConfig :: String -> FieldConfig
quantityConfig defaultValue = makeFieldConfig "Quantity" "Enter quantity" defaultValue
  numberField

thcConfig :: String -> FieldConfig
thcConfig defaultValue = makeFieldConfig "THC %" "Enter THC percentage" defaultValue
  percentageField

cbgConfig :: String -> FieldConfig
cbgConfig defaultValue = makeFieldConfig "CBG %" "Enter CBG percentage" defaultValue
  percentageField

strainConfig :: String -> FieldConfig
strainConfig defaultValue = makeFieldConfig "Strain" "Enter strain name" defaultValue
  requiredText

creatorConfig :: String -> FieldConfig
creatorConfig defaultValue = makeFieldConfig "Creator" "Enter creator name" defaultValue
  requiredText

dominantTarpeneConfig :: String -> FieldConfig
dominantTarpeneConfig defaultValue = makeFieldConfig "Dominant Terpene" "Enter dominant terpene" defaultValue
  requiredText

descriptionConfig :: String -> FieldConfig
descriptionConfig defaultValue = makeFieldConfig "Description" "Enter description" defaultValue multilineText

tagsConfig :: String -> FieldConfig
tagsConfig defaultValue = makeFieldConfig "Tags" "Enter tags (comma-separated)" defaultValue commaListField

effectsConfig :: String -> FieldConfig
effectsConfig defaultValue = makeFieldConfig "Effects" "Enter effects (comma-separated)" defaultValue commaListField

tarpenesConfig :: String -> FieldConfig
tarpenesConfig defaultValue = makeFieldConfig "Terpenes" "Enter terpenes (comma-separated)" defaultValue commaListField

lineageConfig :: String -> FieldConfig
lineageConfig defaultValue = makeFieldConfig "Lineage" "Enter lineage (comma-separated)" defaultValue commaListField

sortConfig :: String -> FieldConfig
sortConfig defaultValue = makeFieldConfig "Sort Order" "Enter sort position" defaultValue numberField

measureUnitConfig :: String -> FieldConfig
measureUnitConfig defaultValue = makeFieldConfig "Measure Unit" "Enter unit (g, mg, etc)" defaultValue
  { validation: allOf [ nonEmpty, alphanumeric ]
  , errorMessage: "Required, valid unit"
  , formatInput: trim
  }

perPackageConfig :: String -> FieldConfig
perPackageConfig defaultValue = makeFieldConfig "Per Package" "Enter amount per package" defaultValue numberField

subcategoryConfig :: String -> FieldConfig
subcategoryConfig defaultValue = makeFieldConfig "Subcategory" "Enter subcategory" defaultValue
  { validation: allOf [ nonEmpty, alphanumeric ]
  , errorMessage: "Required, text only"
  , formatInput: trim
  }

leaflyUrlConfig :: String -> FieldConfig
leaflyUrlConfig defaultValue = makeFieldConfig "Leafly URL" "Enter Leafly URL" defaultValue
  { validation: nonEmpty
  , errorMessage: "Required"
  , formatInput: trim
  }

imgConfig :: String -> FieldConfig
imgConfig defaultValue = makeFieldConfig "Image URL" "Enter image URL" defaultValue
  { validation: nonEmpty
  , errorMessage: "Required"
  , formatInput: trim
  }

inputKls :: String
inputKls =
  """
  rounded-md border-gray-300 shadow-sm
  border-2 mr-2 border-solid
  focus:border-indigo-500 focus:ring-indigo-500
  sm:text-sm
"""

buttonClass :: String -> String
buttonClass color =
  replaceAll (Pattern "COLOR") (Replacement color)
    """
    mb-3 inline-flex items-center rounded-md
    border border-transparent bg-COLOR-600 px-3 py-2
    text-sm font-medium leading-4 text-white shadow-sm
    hover:bg-COLOR-700 focus:outline-none focus:ring-2
    focus:ring-COLOR-500 focus:ring-offset-2
"""-- END FILE

-- FILE: frontend/src/Main.purs
module Main where

import Prelude

import EditItem (editItem)

import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log "Starting main"


  editItem "56b0d1f7-fa3b-4cd4-9e58-79e4724295b0"-- END FILE

-- FILE: frontend/src/MenuLiveView.purs
module MenuLiveView
  ( runLiveView
  ) where

import Prelude

import Data.Array (filter, length, sortBy)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), replace, toLower)
import Data.String.Pattern (Replacement(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.Effect (useState)
import Deku.Hooks ((<#~>))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import MenuLiveView.DataFetcher (FetchConfig, QueryMode(..), defaultConfig, fetchInventory)
import Types (Inventory(..), InventoryResponse(..), ItemCategory, MenuItem(..), StrainLineage(..), Species)

data SortField
  = SortByOrder
  | SortByName
  | SortByCategory
  | SortBySubCategory
  | SortBySpecies
  | SortBySKU
  | SortByPrice
  | SortByQuantity

data SortOrder = Ascending | Descending

type Config =
  { sortFields :: Array (Tuple SortField SortOrder)
  , hideOutOfStock :: Boolean
  , mode :: QueryMode
  , refreshRate :: Int
  , screens :: Int
  , fetchConfig :: FetchConfig
  }

defaultViewConfig :: Config
defaultViewConfig =
  { sortFields:
      [ SortByCategory /\ Ascending
      , SortBySpecies /\ Descending
      , SortByQuantity /\ Descending
      ]
  , hideOutOfStock: true
  , mode: HttpMode
  , refreshRate: 5000
  , screens: 1
  , fetchConfig: defaultConfig
      { apiEndpoint = "http://localhost:8080/inventory"
      , corsHeaders = true
      }
  }

liveView :: Effect Unit
liveView = do
  setInventory /\ inventory <- useState (Inventory [])
  setLoading /\ loading <- useState true
  setError /\ error <- useState ""

  let
    config = defaultViewConfig

    fetchAndUpdateInventory :: Effect Unit
    fetchAndUpdateInventory = launchAff_ do
      liftEffect $ setLoading true
      liftEffect $ setError ""
      liftEffect $ Console.log $ "Fetching inventory with mode: " <> show config.mode

      result <- fetchInventory config.fetchConfig config.mode

      liftEffect $ case result of
        Left err -> do
          Console.error $ "Error fetching inventory: " <> err
          setError err
          setLoading false

        Right (InventoryData inv@(Inventory items)) -> do
          Console.log $ "Received " <> show (length items) <> " items"
          setInventory inv
          setLoading false

        Right (Message msg) -> do
          Console.log msg
          setError msg
          setLoading false


  void fetchAndUpdateInventory


  void $ runInBody Deku.do
    D.div []
      [ D.div
          [ DA.klass_ "status-container" ]
          [ loading <#~> \isLoading ->
              if isLoading then text_ "Loading..."
              else text_ ""
          , error <#~> \err ->
              if err /= "" then text_ ("Error: " <> err)
              else text_ ""
          ]
      , D.div
          [ DA.klass_ "inventory-container" ]
          [ inventory <#~> renderInventory config ]
      ]

renderInventory :: Config -> Inventory -> Nut
renderInventory config (Inventory items) =
  let
    filteredItems =
      if config.hideOutOfStock then filter (\(MenuItem item) -> item.quantity > 0) items
      else items

    sortedItems = sortBy (compareMenuItems config) filteredItems
  in
    D.div
      [ DA.klass_ "inventory-grid" ]
      (map renderItem sortedItems)

renderItem :: MenuItem -> Nut
renderItem (MenuItem record) =
  let
    StrainLineage meta = record.strain_lineage
    className = generateClassName
      { category: record.category
      , subcategory: record.subcategory
      , species: meta.species
      }
  in
    D.div
      [ DA.klass_ ("inventory-item-card " <> className) ]
      [ D.div [ DA.klass_ "item-header" ]
          [ D.div []
              [ D.div [ DA.klass_ "item-brand" ] [ text_ record.brand ]
              , D.div [ DA.klass_ "item-name" ] [ text_ ("'" <> record.name <> "'") ]
              ]
          , D.div [ DA.klass_ "item-img" ]
              [ D.img [ DA.alt_ "product image", DA.src_ meta.img ] [] ]
          ]
      , D.div [ DA.klass_ "item-category" ]
          [ text_ (show record.category <> " - " <> record.subcategory) ]
      , D.div [ DA.klass_ "item-species" ]
          [ text_ ("Species: " <> show meta.species) ]
      , D.div [ DA.klass_ "item-strain_lineage" ]
          [ text_ ("Strain: " <> meta.strain) ]
      , D.div [ DA.klass_ "item-price" ]
          [ text_ ("$" <> show record.price <> " (" <> record.per_package <> "" <> record.measure_unit <> ")") ]
      , D.div [ DA.klass_ "item-quantity" ]
          [ text_ ("in stock: " <> show record.quantity) ]
      ]

generateClassName :: { category :: ItemCategory, subcategory :: String, species :: Species } -> String
generateClassName item =
  "species-" <> toClassName (show item.species)
    <> " category-"
    <> toClassName (show item.category)
    <> " subcategory-"
    <> toClassName item.subcategory

toClassName :: String -> String
toClassName str = toLower (replace (Pattern " ") (Replacement "-") str)

compareMenuItems :: Config -> MenuItem -> MenuItem -> Ordering
compareMenuItems config (MenuItem item1) (MenuItem item2) =
  let
    StrainLineage meta1 = item1.strain_lineage
    StrainLineage meta2 = item2.strain_lineage

    compareByField :: Tuple SortField SortOrder -> Ordering
    compareByField (sortField /\ sortOrder) =
      let
        fieldComparison = case sortField of
          SortByOrder -> compare item1.sort item2.sort
          SortByName -> compare item1.name item2.name
          SortByCategory -> compare item1.category item2.category
          SortBySubCategory -> compare item1.subcategory item2.subcategory
          SortBySpecies -> compare meta1.species meta2.species
          SortBySKU -> compare item1.sku item2.sku
          SortByPrice -> compare item1.price item2.price
          SortByQuantity -> compare item1.quantity item2.quantity
      in
        case sortOrder of
          Ascending -> fieldComparison
          Descending -> invertOrdering fieldComparison

    compareWithPriority :: Array (Tuple SortField SortOrder) -> Ordering
    compareWithPriority priorities = case Array.uncons priorities of
      Nothing -> EQ
      Just { head: priority, tail: rest } ->
        case compareByField priority of
          EQ -> compareWithPriority rest
          result -> result
  in
    compareWithPriority config.sortFields

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT

runLiveView :: Effect Unit
runLiveView = do
  Console.log "Starting MenuLiveView with backend integration"
  liveView-- END FILE

-- FILE: frontend/src/Route.purs
module Route where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Duplex (RouteDuplex', root, segment, string)
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = LiveView
  | Create
  | Edit String

derive instance Eq Route
derive instance Ord Route
derive instance genericRoute :: Generic Route _

instance Show Route where
  show = genericShow

route :: RouteDuplex' Route
route = root $ G.sum
  { "LiveView": G.noArgs
  , "Create": "create" / G.noArgs
  , "Edit": "edit" / (string segment)
  }-- END FILE

-- FILE: frontend/src/Types.purs
module Types where

import Prelude

import Control.Monad.Except (ExceptT)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Int (fromString)
import Data.Int as Int
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Number (fromString) as Number
import Data.String (trim)
import Deku.Attribute (Attribute)
import Deku.Control (elementify)
import Deku.Core (Nut, attributeAtYourOwnRisk)
import Effect (Effect)
import FRP.Poll (Poll)
import Foreign (Foreign, ForeignError(..), F, fail)
import Foreign.Index (readProp)
import Type.Proxy (Proxy(..))
import UUID (UUID, parseUUID)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Foreign (Foreign, F, ForeignError(..), fail, typeOf)
import Foreign.Index (readProp)
import Data.List.NonEmpty (NonEmptyList)
import Control.Monad.Except.Trans (ExceptT)
import Data.Identity (Identity)

newtype ForeignRequestBody = ForeignRequestBody Foreign

data InventoryResponse
  = InventoryData Inventory
  | Message String

newtype Inventory = Inventory (Array MenuItem)

type MenuItemRecord =
  { sort :: Int
  , sku :: UUID
  , brand :: String
  , name :: String
  , price :: Number
  , measure_unit :: String
  , per_package :: String
  , quantity :: Int
  , category :: ItemCategory
  , subcategory :: String
  , description :: String
  , tags :: Array String
  , effects :: Array String
  , strain_lineage :: StrainLineage
  }

newtype MenuItem = MenuItem MenuItemRecord

derive instance Newtype MenuItem _

data ItemCategory
  = Flower
  | PreRolls
  | Vaporizers
  | Edibles
  | Drinks
  | Concentrates
  | Topicals
  | Tinctures
  | Accessories

derive instance eqItemCategory :: Eq ItemCategory
derive instance ordItemCategory :: Ord ItemCategory

data StrainLineage = StrainLineage
  { thc :: String
  , cbg :: String
  , strain :: String
  , creator :: String
  , species :: Species
  , dominant_tarpene :: String
  , tarpenes :: Array String
  , lineage :: Array String
  , leafly_url :: String
  , img :: String
  }

derive instance genericStrainLineage :: Generic StrainLineage _

data Species
  = Indica
  | IndicaDominantHybrid
  | Hybrid
  | SativaDominantHybrid
  | Sativa

derive instance eqItemSpecies :: Eq Species
derive instance ordItemSpecies :: Ord Species

type HTMLFormField (r :: Row Type) =
  ( __tag :: Proxy "HTMLFormField"
  , value :: String
  , validation :: ValidationRule
  , onUpdate :: String -> Effect Unit
  | r
  )

formField
  :: forall r
   . Array (Poll (Attribute (HTMLFormField r)))
  -> Array Nut
  -> Nut
formField = elementify Nothing "div"

formFieldValue
  :: forall r
   . Poll String
  -> Poll (Attribute (value :: String | r))
formFieldValue = map (attributeAtYourOwnRisk "value")

formFieldValidation
  :: forall r
   . Poll ValidationRule
  -> Poll (Attribute (validation :: ValidationRule | r))
formFieldValidation = map \rule ->
  attributeAtYourOwnRisk "data-validation" (show rule)

type MenuItemFormInput =
  { sort :: String
  , sku :: String
  , brand :: String
  , name :: String
  , price :: String
  , measure_unit :: String
  , per_package :: String
  , quantity :: String
  , category :: String
  , subcategory :: String
  , description :: String
  , tags :: String
  , effects :: String
  , strain_lineage :: StrainLineageFormInput
  }

type StrainLineageFormInput =
  { thc :: String
  , cbg :: String
  , strain :: String
  , creator :: String
  , species :: String
  , dominant_tarpene :: String
  , tarpenes :: String
  , lineage :: String
  , leafly_url :: String
  , img :: String
  }

type FieldConfig = Record (FieldConfigRow ())

newtype FieldConfigRecord r = FieldConfigRecord (Record (FieldConfigRow r))

type FieldConfigRow r =
  ( label :: String
  , placeholder :: String
  , defaultValue :: String
  , validation :: ValidationRule
  , errorMessage :: String
  , formatInput :: String -> String
  | r
  )

type DropdownConfig =
  { label :: String
  , options :: Array { value :: String, label :: String }
  , defaultValue :: String
  }

type TextFieldConfig r =
  ( maxLength :: Int
  | FieldConfigRow r
  )

type NumberFieldConfig r =
  ( min :: Number
  , max :: Number
  | FieldConfigRow r
  )

toFieldConfigRecord :: forall r. Record (FieldConfigRow r) -> FieldConfigRecord r
toFieldConfigRecord = FieldConfigRecord

fromFieldConfigRecord :: forall r. FieldConfigRecord r -> Record (FieldConfigRow r)
fromFieldConfigRecord (FieldConfigRecord record) = record

data ValidationResult a
  = ValidationSuccess a
  | ValidationError String

newtype ValidationRule = ValidationRule (String -> Boolean)

newtype Validated a = Validated a

derive instance genericValidated :: Generic (Validated a) _
derive instance functorValidated :: Functor Validated

class FormValue a where
  fromFormValue :: String -> ValidationResult a

class FieldValidator a where
  validateField :: String -> Either String a
  validationError :: Proxy a -> String

type ValidationPreset =
  { validation :: ValidationRule
  , errorMessage :: String
  , formatInput :: String -> String
  }

mkValidationRule :: (String -> Boolean) -> ValidationRule
mkValidationRule = ValidationRule

runValidation :: ValidationRule -> String -> Boolean
runValidation (ValidationRule f) = f

instance Enum ItemCategory where
  succ Flower = Just PreRolls
  succ PreRolls = Just Vaporizers
  succ Vaporizers = Just Edibles
  succ Edibles = Just Drinks
  succ Drinks = Just Concentrates
  succ Concentrates = Just Topicals
  succ Topicals = Just Tinctures
  succ Tinctures = Just Accessories
  succ Accessories = Nothing

  pred PreRolls = Just Flower
  pred Vaporizers = Just PreRolls
  pred Edibles = Just Vaporizers
  pred Drinks = Just Edibles
  pred Concentrates = Just Drinks
  pred Topicals = Just Concentrates
  pred Tinctures = Just Topicals
  pred Accessories = Just Tinctures
  pred Flower = Nothing

instance Bounded ItemCategory where
  bottom = Flower
  top = Accessories

instance BoundedEnum ItemCategory where
  cardinality = Cardinality 9
  fromEnum Flower = 0
  fromEnum PreRolls = 1
  fromEnum Vaporizers = 2
  fromEnum Edibles = 3
  fromEnum Drinks = 4
  fromEnum Concentrates = 5
  fromEnum Topicals = 6
  fromEnum Tinctures = 7
  fromEnum Accessories = 8

  toEnum 0 = Just Flower
  toEnum 1 = Just PreRolls
  toEnum 2 = Just Vaporizers
  toEnum 3 = Just Edibles
  toEnum 4 = Just Drinks
  toEnum 5 = Just Concentrates
  toEnum 6 = Just Topicals
  toEnum 7 = Just Tinctures
  toEnum 8 = Just Accessories
  toEnum _ = Nothing

instance Show ItemCategory where
  show Flower = "Flower"
  show PreRolls = "PreRolls"
  show Vaporizers = "Vaporizers"
  show Edibles = "Edibles"
  show Drinks = "Drinks"
  show Concentrates = "Concentrates"
  show Topicals = "Topicals"
  show Tinctures = "Tinctures"
  show Accessories = "Accessories"

instance Enum Species where
  succ Indica = Just IndicaDominantHybrid
  succ IndicaDominantHybrid = Just Hybrid
  succ Hybrid = Just SativaDominantHybrid
  succ SativaDominantHybrid = Just Sativa
  succ Sativa = Nothing

  pred IndicaDominantHybrid = Just Indica
  pred Hybrid = Just IndicaDominantHybrid
  pred SativaDominantHybrid = Just Hybrid
  pred Sativa = Just SativaDominantHybrid
  pred Indica = Nothing

instance Bounded Species where
  bottom = Indica
  top = Sativa

instance BoundedEnum Species where
  cardinality = Cardinality 5
  fromEnum Indica = 0
  fromEnum IndicaDominantHybrid = 1
  fromEnum Hybrid = 2
  fromEnum SativaDominantHybrid = 3
  fromEnum Sativa = 4

  toEnum 0 = Just Indica
  toEnum 1 = Just IndicaDominantHybrid
  toEnum 2 = Just Hybrid
  toEnum 3 = Just SativaDominantHybrid
  toEnum 4 = Just Sativa
  toEnum _ = Nothing

instance Show Species where
  show Indica = "Indica"
  show IndicaDominantHybrid = "IndicaDominantHybrid"
  show Hybrid = "Hybrid"
  show SativaDominantHybrid = "SativaDominantHybrid"
  show Sativa = "Sativa"

instance writeForeignMenuItem :: WriteForeign MenuItem where
  writeImpl (MenuItem item) = writeImpl
    { sort: item.sort
    , sku: item.sku
    , brand: item.brand
    , name: item.name
    , price: item.price
    , measure_unit: item.measure_unit
    , per_package: item.per_package
    , quantity: item.quantity
    , category: show item.category
    , subcategory: item.subcategory
    , description: item.description
    , tags: item.tags
    , effects: item.effects
    , strain_lineage: item.strain_lineage
    }

instance writeForeignStrainLineage :: WriteForeign StrainLineage where
  writeImpl (StrainLineage lineage) = writeImpl
    { thc: lineage.thc
    , cbg: lineage.cbg
    , strain: lineage.strain
    , creator: lineage.creator
    , species: show lineage.species
    , dominant_tarpene: lineage.dominant_tarpene
    , tarpenes: lineage.tarpenes
    , lineage: lineage.lineage
    , leafly_url: lineage.leafly_url
    , img: lineage.img
    }

instance writeForeignInventory :: WriteForeign Inventory where
  writeImpl (Inventory items) = writeImpl items

instance writeForeignSpecies :: WriteForeign Species where
  writeImpl = writeImpl <<< show

instance writeForeignFieldConfigRecord :: WriteForeign (FieldConfigRecord r) where
  writeImpl (FieldConfigRecord config) = writeImpl
    { label: config.label
    , placeholder: config.placeholder
    , validation: config.validation
    , errorMessage: config.errorMessage
    , formatInput: "<format function>"
    }

instance writeForeignInventoryResponse :: WriteForeign InventoryResponse where
  writeImpl (InventoryData inventory) = writeImpl { type: "data", value: inventory }
  writeImpl (Message msg) = writeImpl { type: "message", value: msg }

instance writeForeignValidationRule :: WriteForeign ValidationRule where
  writeImpl _ = writeImpl "<validation function>"

instance readForeignMenuItem :: ReadForeign MenuItem where
  readImpl json = do
    sort <- readProp "sort" json >>= readImpl
    skuStr <- readProp "sku" json >>= readImpl
    sku <- case parseUUID skuStr of
      Just uuid -> pure uuid
      Nothing -> fail $ ForeignError "Invalid UUID format for sku"
    brand <- readProp "brand" json >>= readImpl
    name <- readProp "name" json >>= readImpl
    price <- readProp "price" json >>= readImpl
    measure_unit <- readProp "measure_unit" json >>= readImpl
    per_package <- readProp "per_package" json >>= readImpl
    quantity <- readProp "quantity" json >>= readImpl
    categoryStr <- readProp "category" json >>= readImpl
    category <- case categoryStr of
      "Flower" -> pure Flower
      "PreRolls" -> pure PreRolls
      "Vaporizers" -> pure Vaporizers
      "Edibles" -> pure Edibles
      "Drinks" -> pure Drinks
      "Concentrates" -> pure Concentrates
      "Topicals" -> pure Topicals
      "Tinctures" -> pure Tinctures
      "Accessories" -> pure Accessories
      _ -> fail (ForeignError "Invalid ItemCategory value")
    subcategory <- readProp "subcategory" json >>= readImpl
    description <- readProp "description" json >>= readImpl
    tags <- readProp "tags" json >>= readImpl
    effects <- readProp "effects" json >>= readImpl
    strain_lineage <- readProp "strain_lineage" json >>= readImpl
    pure $ MenuItem
      { sort
      , sku
      , brand
      , name
      , price
      , measure_unit
      , per_package
      , quantity
      , category
      , subcategory
      , description
      , tags
      , effects
      , strain_lineage
      }

instance readForeignInventory :: ReadForeign Inventory where
  readImpl json = do
    items <- readImpl json :: F (Array MenuItem)
    pure $ Inventory items

instance readForeignSpecies :: ReadForeign Species where
  readImpl json = do
    str <- readImpl json
    case str of
      "Indica" -> pure Indica
      "IndicaDominantHybrid" -> pure IndicaDominantHybrid
      "Hybrid" -> pure Hybrid
      "SativaDominantHybrid" -> pure SativaDominantHybrid
      "Sativa" -> pure Sativa
      _ -> fail (ForeignError "Invalid Species value")

instance readForeignStrainLineage :: ReadForeign StrainLineage where
  readImpl json = do
    thc <- readProp "thc" json >>= readImpl
    cbg <- readProp "cbg" json >>= readImpl
    strain <- readProp "strain" json >>= readImpl
    creator <- readProp "creator" json >>= readImpl
    species <- readProp "species" json >>= readImpl
    dominant_tarpene <- readProp "dominant_tarpene" json >>= readImpl
    tarpenes <- readProp "tarpenes" json >>= readImpl
    lineage <- readProp "lineage" json >>= readImpl
    leafly_url <- readProp "leafly_url" json >>= readImpl
    img <- readProp "img" json >>= readImpl
    pure $ StrainLineage
      { thc
      , cbg
      , strain
      , creator
      , species
      , dominant_tarpene
      , tarpenes
      , lineage
      , leafly_url
      , img
      }

instance readForeignInventoryResponse :: ReadForeign InventoryResponse where
  readImpl f = do
    obj <- readImpl f
    case obj of

      array | isArray array -> do
        inventory <- readImpl array :: F Inventory
        pure $ InventoryData inventory


      _ -> do
        typeField <- readProp "type" obj >>= readImpl :: F String
        case typeField of
          "data" -> do
            value <- readProp "value" obj >>= readImpl :: F Inventory
            pure $ InventoryData value
          "message" -> do
            value <- readProp "value" obj >>= readImpl :: F String
            pure $ Message value
          _ -> fail $ ForeignError "Invalid response type"
    where
    isArray :: Foreign -> Boolean
    isArray value = typeOf value == "array"

derive instance Generic MenuItem _

instance showMenuItem :: Show MenuItem where
  show (MenuItem item) =
    "{ name: " <> show item.name
      <> ", brand: "
      <> show item.brand
      <> ", quantity: "
      <> show item.quantity
      <> " }"

instance showStrainLineage :: Show StrainLineage where
  show (StrainLineage lineage) =
    "{ strain: " <> show lineage.strain
      <> ", species: "
      <> show lineage.species
      <> " }"

instance showValidationRule :: Show ValidationRule where
  show _ = "<validation function>"

instance formValueString :: FormValue String where
  fromFormValue = ValidationSuccess <<< trim

instance formValueNumber :: FormValue Number where
  fromFormValue str = case Number.fromString (trim str) of
    Just n -> ValidationSuccess n
    Nothing -> ValidationError "Invalid number format"

instance formValueInt :: FormValue Int where
  fromFormValue str = case Int.fromString (trim str) of
    Just n -> ValidationSuccess n
    Nothing -> ValidationError "Invalid integer format"

instance formValueItemCategory :: FormValue ItemCategory where
  fromFormValue str = case str of
    "Flower" -> ValidationSuccess Flower
    "PreRolls" -> ValidationSuccess PreRolls
    "Vaporizers" -> ValidationSuccess Vaporizers
    "Edibles" -> ValidationSuccess Edibles
    "Drinks" -> ValidationSuccess Drinks
    "Concentrates" -> ValidationSuccess Concentrates
    "Topicals" -> ValidationSuccess Topicals
    "Tinctures" -> ValidationSuccess Tinctures
    "Accessories" -> ValidationSuccess Accessories
    _ -> ValidationError "Invalid category value"

instance formValueSpecies :: FormValue Species where
  fromFormValue str = case str of
    "Indica" -> ValidationSuccess Indica
    "IndicaDominantHybrid" -> ValidationSuccess IndicaDominantHybrid
    "Hybrid" -> ValidationSuccess Hybrid
    "SativaDominantHybrid" -> ValidationSuccess SativaDominantHybrid
    "Sativa" -> ValidationSuccess Sativa
    _ -> ValidationError "Invalid species value"

instance formValueUUID :: FormValue UUID where
  fromFormValue str = case parseUUID (trim str) of
    Just uuid -> ValidationSuccess uuid
    Nothing -> ValidationError "Invalid UUID format"

instance formValueValidated :: (FieldValidator a) => FormValue (Validated a) where
  fromFormValue str = case validateField str of
    Right value -> ValidationSuccess value
    Left err -> ValidationError err

instance fieldValidatorValidated :: (FieldValidator a) => FieldValidator (Validated a) where
  validateField str = do
    result <- validateField str
    pure $ Validated result
  validationError _ = "Validated: " <> validationError (Proxy :: Proxy a)

instance fieldValidatorString :: FieldValidator String where
  validateField str = Right (trim str)
  validationError _ = "Invalid string format"

instance fieldValidatorNumber :: FieldValidator Number where
  validateField str = case Number.fromString (trim str) of
    Just n ->
      if n >= 0.0 then Right n
      else Left "Must be a positive number"
    Nothing -> Left "Must be a valid number"
  validationError _ = "Must be a valid number"

instance fieldValidatorInt :: FieldValidator Int where
  validateField str = case fromString (trim str) of
    Just n -> Right n
    Nothing -> Left "Must be a valid integer"
  validationError _ = "Must be a valid integer"

instance fieldValidatorUUID :: FieldValidator UUID where
  validateField str = case parseUUID (trim str) of
    Just uuid -> Right uuid
    Nothing -> Left "Must be a valid UUID"
  validationError _ = "Invalid UUID format"

instance fieldValidatorItemCategory :: FieldValidator ItemCategory where
  validateField str = case fromFormValue str of
    ValidationSuccess cat -> Right cat
    ValidationError err -> Left err
  validationError _ = "Must be a valid category"

instance fieldValidatorSpecies :: FieldValidator Species where
  validateField str = case fromFormValue str of
    ValidationSuccess species -> Right species
    ValidationError err -> Left err
  validationError _ = "Must be a valid species"-- END FILE

-- FILE: frontend/src/Utils.purs
module Utils where

import Prelude

import Data.Array (catMaybes, filter, range, replicate, (!!))
import Data.Array (length) as Array
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Int (floor, fromString, toNumber) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Number
import Data.String (Pattern(..), joinWith, length, split, take, trim)
import Data.String (length) as String
import Effect (Effect)
import Effect.Random (random)

randomInt :: Int -> Int -> Effect Int
randomInt min max = do
  r <- random
  pure $ Int.floor $ r * Int.toNumber (max - min + 1) + Int.toNumber min

ensureNumber :: String -> String
ensureNumber str = fromMaybe "0.0" $ map show $ Number.fromString $ trim str

ensureInt :: String -> String
ensureInt str = fromMaybe "0" $ map show $ Int.fromString $ trim str

padStart :: Int -> String -> String
padStart targetLength str =
  let
    paddingLength = max 0 (targetLength - length str)
    padding = replicate paddingLength "0"
  in
    joinWith "" padding <> str

parseCommaList :: String -> Array String
parseCommaList str =
  if str == "" then []
  else
    str
      # split (Pattern ",")
      # map trim
      # filter (_ /= "")

formatDollarAmount :: String -> String
formatDollarAmount str =
  if str == "" then ""
  else case Number.fromString str of
    Just n ->
      let
        fixed = show n
        parts = split (Pattern ".") fixed
      in
        case Array.length parts of
          1 -> fixed <> ".00"
          2 ->
            let
              decimals = fromMaybe "" $ parts !! 1
            in
              if String.length decimals >= 2 then fromMaybe "" (parts !! 0) <> "." <> take 2 decimals
              else fromMaybe "" (parts !! 0) <> "." <> decimals <> "0"
          _ -> str
    Nothing -> str

getAllEnumValues :: ∀ a. BoundedEnum a => Bounded a => Array a
getAllEnumValues = catMaybes $ map toEnum $ range 0 (fromEnum (top :: a))-- END FILE

-- FILE: frontend/src/UUID.purs
module UUID where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (hexadecimal, toStringAs)
import Data.Int.Bits ((.|.))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (joinWith)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Effect (Effect)
import Utils (padStart, randomInt)
import Yoga.JSON (class WriteForeign, writeImpl)

newtype UUID = UUID String

derive instance genericUUID :: Generic UUID _
derive instance newtypeUUID :: Newtype UUID _
instance showUUID :: Show UUID where
  show (UUID uuid) = uuid

derive instance eqUUID :: Eq UUID
derive instance ordUUID :: Ord UUID

instance writeForeignUUID :: WriteForeign UUID where
  writeImpl (UUID str) = writeImpl str

parseUUID :: String -> Maybe UUID
parseUUID str =
  case regex "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$" noFlags of
    Left _ -> Nothing
    Right r ->
      if test r str then Just $ UUID str
      else Nothing

uuidToString :: UUID -> String
uuidToString (UUID uuid) = uuid

emptyUUID :: UUID
emptyUUID = UUID "00000000-0000-0000-0000-000000000000"

genUUID :: Effect UUID
genUUID = do

  r1 <- randomInt 0 0xFFFF
  r2 <- randomInt 0 0xFFFF
  r3 <- randomInt 0 0xFFFF
  r4 <- randomInt 0 0x0FFF
  r5 <- randomInt 0 0x3FFF
  r6 <- randomInt 0 0xFFFF
  r7 <- randomInt 0 0xFFFF
  r8 <- randomInt 0 0xFFFF


  let
    versioned = r4 .|. 0x4000
    variant = r5 .|. 0x8000


  let
    hex1 = padStart 4 (toHex r1) <> padStart 4 (toHex r2)
    hex2 = padStart 4 (toHex r3)
    hex3 = padStart 4 (toHex versioned)
    hex4 = padStart 4 (toHex variant)
    hex5 = padStart 4 (toHex r6) <> padStart 4 (toHex r7) <> padStart 4 (toHex r8)
    uuid = joinWith "-" [ hex1, hex2, hex3, hex4, hex5 ]

  pure $ UUID uuid
  where
  toHex = toStringAs hexadecimal
-- END FILE

-- FILE: frontend/src/Validation.purs
module Validation where

import Prelude
import Types (class FieldValidator, class FormValue, FieldConfigRow, HTMLFormField, ItemCategory, MenuItem(..), MenuItemFormInput, NumberFieldConfig, StrainLineage(..), StrainLineageFormInput, TextFieldConfig, ValidationPreset, ValidationResult(..), ValidationRule(..), fromFormValue, runValidation, validationError)

import Data.Array (all)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Number
import Data.String (length) as String
import Data.String (trim)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Type.Proxy (Proxy(..))
import UUID (UUID, parseUUID)
import Utils (formatDollarAmount, parseCommaList)
import Types

requireValid :: forall a. String -> ValidationResult a -> Either String a
requireValid field = case _ of
  ValidationSuccess x -> Right x
  ValidationError err -> Left $ field <> ": " <> err

requiredField :: forall a. FormValue a => FieldValidator a => ValidationRule
requiredField = ValidationRule \str ->
  let
    validate :: String -> Either String a
    validate = validateField
  in
    case validate str of
      Right _ -> true
      Left _ -> false

nonEmpty :: ValidationRule
nonEmpty = ValidationRule (_ /= "")

validUUID :: ValidationRule
validUUID = ValidationRule \str -> case parseUUID (trim str) of
  Just _ -> true
  Nothing -> false

alphanumeric :: ValidationRule
alphanumeric = ValidationRule \str -> case regex "^[A-Za-z0-9-\\s]+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

percentage :: ValidationRule
percentage = ValidationRule \str -> case regex "^\\d{1,3}(\\.\\d{1,2})?%$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

dollarAmount :: ValidationRule
dollarAmount = ValidationRule \str -> case Number.fromString str of
  Just n -> n >= 0.0
  Nothing -> false

positiveInteger :: ValidationRule
positiveInteger = ValidationRule \str -> case fromString str of
  Just n -> n > 0
  Nothing -> false

vowels :: ValidationRule
vowels = ValidationRule \str -> case regex "^[AEIOUYaeiouy\\s]+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

consonants :: ValidationRule
consonants = ValidationRule \str -> case regex "^[BCDFGHJKLMNPQRSTVWXZbcdfghjklmnpqrstvwxz\\s]+$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

commaList :: ValidationRule
commaList = ValidationRule \str -> case regex "^[^,]+(,[^,]+)*$" noFlags of
  Left _ -> false
  Right validRegex -> test validRegex str

maxLength :: Int -> ValidationRule
maxLength n = ValidationRule \str -> String.length str <= n

allOf :: Array ValidationRule -> ValidationRule
allOf rules = ValidationRule \str ->
  all (\(ValidationRule rule) -> rule str) rules

requiredText :: ValidationPreset
requiredText =
  { validation: allOf [ nonEmpty, alphanumeric ]
  , errorMessage: "Required, text only"
  , formatInput: trim
  }

requiredTextWithLimit :: Int -> ValidationPreset
requiredTextWithLimit limit =
  { validation: allOf [ nonEmpty, alphanumeric, maxLength limit ]
  , errorMessage: "Required, text only (max " <> show limit <> " chars)"
  , formatInput: trim
  }

percentageField :: ValidationPreset
percentageField =
  { validation: percentage
  , errorMessage: "Required format: XX.XX%"
  , formatInput: trim
  }

moneyField :: ValidationPreset
moneyField =
  { validation: allOf [ nonEmpty, dollarAmount ]
  , errorMessage: "Required, valid dollar amount"
  , formatInput: formatDollarAmount
  }

numberField :: ValidationPreset
numberField =
  { validation: allOf [ nonEmpty, positiveInteger ]
  , errorMessage: "Required, positive whole number"
  , formatInput: \str -> fromMaybe str $ map show $ fromString str
  }

commaListField :: ValidationPreset
commaListField =
  { validation: commaList
  , errorMessage: "Must be a comma-separated list"
  , formatInput: trim
  }

multilineText :: ValidationPreset
multilineText =
  { validation: nonEmpty
  , errorMessage: "Required"
  , formatInput: identity
  }

validateForm :: forall r. Record (FieldConfigRow r) -> MenuItemFormInput -> Either String MenuItem
validateForm _ input = do
  name <- validateTextField
    { label: "Name"
    , maxLength: 50
    , placeholder: "Enter name"
    , defaultValue: ""
    , validation: allOf [ nonEmpty, alphanumeric ]
    , errorMessage: "Required, text only (max 50 chars)"
    , formatInput: trim
    }
    input.name

  sku <- requireValid "SKU" $ fromFormValue input.sku
  brand <- requireValid "Brand" $ fromFormValue input.brand
  price <- requireValid "Price" $ fromFormValue input.price
  quantity <- requireValid "Quantity" $ fromFormValue input.quantity
  category <- requireValid "Category" $ fromFormValue input.category

  strainLineage <- validateStrainLineage input.strain_lineage

  pure $ MenuItem
    { sort: 0
    , sku
    , brand
    , name
    , price
    , measure_unit: "units"
    , per_package: show quantity
    , quantity
    , category
    , subcategory: show category
    , description: input.description
    , tags: parseCommaList input.tags
    , effects: parseCommaList input.effects
    , strain_lineage: strainLineage
    }

validateTextField :: forall r. Record (TextFieldConfig r) -> String -> Either String String
validateTextField config input = do
  let ValidationRule validate = config.validation
  if not (validate input) then Left config.errorMessage
  else if String.length input > config.maxLength then Left $ "Must be less than " <> show config.maxLength <> " characters"
  else Right $ config.formatInput input

validateNumberField :: forall r. Record (NumberFieldConfig r) -> String -> Either String Number
validateNumberField config input =
  case validateField input of
    Right value ->
      if value >= config.min && value <= config.max then Right value
      else Left $ "Must be between " <> show config.min <> " and " <> show config.max
    Left err -> Left err

validateFormField
  :: forall r a
   . FormValue a
  => FieldValidator a
  => Record (HTMLFormField r)
  -> ValidationResult a
validateFormField field =
  let
    validationResult = fromFormValue field.value
  in
    case validationResult of
      ValidationSuccess value ->
        if runValidation field.validation field.value then ValidationSuccess value
        else ValidationError (validationError (Proxy :: Proxy a))
      ValidationError err -> ValidationError err

validateField :: forall a. FormValue a => FieldValidator a => String -> Either String a
validateField str = do
  let trimmed = trim str
  if trimmed == "" then Left (validationError (Proxy :: Proxy a))
  else case fromFormValue trimmed of
    ValidationSuccess value -> Right value
    ValidationError err -> Left err

validateStringField :: String -> ValidationRule -> String -> Either String String
validateStringField fieldName (ValidationRule rule) value =
  if rule value then Right value
  else Left $ fieldName <> " validation failed"

validateMenuItem :: MenuItemFormInput -> Either String MenuItem
validateMenuItem input = do
  sort <- validateStringField "Sort" positiveInteger input.sort
  sku <- validateStringField "SKU" (allOf [ nonEmpty, validUUID ]) input.sku
  brand <- validateStringField "Brand" (allOf [ nonEmpty, alphanumeric ]) input.brand
  name <- validateStringField "Name" (allOf [ nonEmpty, alphanumeric ]) input.name
  price <- validateStringField "Price" dollarAmount input.price
  measure_unit <- validateStringField "Measure Unit" nonEmpty input.measure_unit
  per_package <- validateStringField "Per Package" nonEmpty input.per_package
  quantity <- validateStringField "Quantity" positiveInteger input.quantity
  category <- validateStringField "Category" nonEmpty input.category
  subcategory <- validateStringField "Subcategory" nonEmpty input.subcategory


  strainLineage <- validateStrainLineage input.strain_lineage


  sortNum <- case fromString sort of
    Just n -> Right n
    Nothing -> Left "Invalid sort number"

  skuUUID <- case parseUUID sku of
    Just uuid -> Right uuid
    Nothing -> Left "Invalid UUID format"

  priceNum <- case Number.fromString price of
    Just n -> Right n
    Nothing -> Left "Invalid price format"

  quantityNum <- case fromString quantity of
    Just n -> Right n
    Nothing -> Left "Invalid quantity format"

  categoryType <- validateCategory category

  pure $ MenuItem
    { sort: sortNum
    , sku: skuUUID
    , brand
    , name
    , price: priceNum
    , measure_unit
    , per_package
    , quantity: quantityNum
    , category: categoryType
    , subcategory
    , description: input.description
    , tags: parseCommaList input.tags
    , effects: parseCommaList input.effects
    , strain_lineage: strainLineage
    }

validateStrainLineage :: StrainLineageFormInput -> Either String StrainLineage
validateStrainLineage input = do
  thc <- validateStringField "THC" percentage input.thc
  cbg <- validateStringField "CBG" percentage input.cbg
  strain <- validateStringField "Strain" (allOf [ nonEmpty, alphanumeric ]) input.strain
  creator <- validateStringField "Creator" (allOf [ nonEmpty, alphanumeric ]) input.creator
  species <- validateStringField "Species" nonEmpty input.species
  dominant_tarpene <- validateStringField "Dominant Terpene" (allOf [ nonEmpty, alphanumeric ]) input.dominant_tarpene
  leafly_url <- validateStringField "Leafly URL" nonEmpty input.leafly_url
  img <- validateStringField "Image URL" nonEmpty input.img

  speciesType <- validateSpecies species

  pure $ StrainLineage
    { thc
    , cbg
    , strain
    , creator
    , species: speciesType
    , dominant_tarpene
    , tarpenes: parseCommaList input.tarpenes
    , lineage: parseCommaList input.lineage
    , leafly_url
    , img
    }

validateCategory :: String -> Either String ItemCategory
validateCategory = case _ of
  "Flower" -> Right Flower
  "PreRolls" -> Right PreRolls
  "Vaporizers" -> Right Vaporizers
  "Edibles" -> Right Edibles
  "Drinks" -> Right Drinks
  "Concentrates" -> Right Concentrates
  "Topicals" -> Right Topicals
  "Tinctures" -> Right Tinctures
  "Accessories" -> Right Accessories
  _ -> Left "Invalid category"

validateSpecies :: String -> Either String Species
validateSpecies = case _ of
  "Indica" -> Right Indica
  "IndicaDominantHybrid" -> Right IndicaDominantHybrid
  "Hybrid" -> Right Hybrid
  "SativaDominantHybrid" -> Right SativaDominantHybrid
  "Sativa" -> Right Sativa
  _ -> Left "Invalid species"-- END FILE

