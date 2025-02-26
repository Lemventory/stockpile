module Main where

import Prelude
import Types

import API (fetchInventory, readInventory)
import Control.Monad.ST.Class (liftST)
import CreateItem (createItem)
import Data.Array (find, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Deku.Core (fixed)
import Deku.DOM as D
import Deku.Hooks (cycle)
import Deku.Toplevel (runInBody)
import EditItem (editItem, renderError)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import FRP.Poll as Poll
import MenuLiveView (createMenuLiveView)
import Route (Route(..), nav, route)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Types.LiveViewConfig (defaultViewConfig)
import UUIDGen (genUUID)

testItemUUID :: String
testItemUUID = "56b0d1f7-fa3b-4cd4-9e58-79e4724295b0"

main :: Effect Unit
main = do
  Console.log "Application starting"

  currentRoute <- liftST Poll.create
  inventoryState <- liftST Poll.create
  loadingState <- liftST Poll.create
  errorState <- liftST Poll.create

  let menuLiveView = createMenuLiveView inventoryState.poll loadingState.poll errorState.poll

  let
    matcher _ r = do
      Console.log $ "Route changed to: " <> show r

      case r of
        LiveView -> do
          currentRoute.push $ Tuple r menuLiveView

          loadingState.push true
          errorState.push ""

          launchAff_ do
            liftEffect $ Console.log "Loading inventory data..."
            result <- fetchInventory defaultViewConfig.fetchConfig defaultViewConfig.mode

            liftEffect $ case result of
              Left err -> do
                Console.error $ "Error fetching inventory: " <> err
                loadingState.push false
                errorState.push $ "Error: " <> err

              Right (InventoryData inv) -> do
                Console.log $ "Loaded inventory successfully"
                inventoryState.push inv
                loadingState.push false

              Right (Message msg) -> do
                Console.log $ "Received message: " <> msg
                loadingState.push false
                errorState.push msg

        Create -> do
          -- Generate a new UUID for the Create page
          newUUID <- genUUID
          let newUUIDStr = show newUUID
          Console.log $ "Generated new UUID for Create page: " <> newUUIDStr

          -- Create the component with the UUID string
          currentRoute.push $ Tuple r (createItem newUUIDStr)

        Edit uuid -> do
          let actualUuid = if uuid == "test" then testItemUUID else uuid
          Console.log $ "Loading item with UUID: " <> actualUuid
          
          -- Fetch the item before creating the component
          loadingState.push true
          launchAff_ do
            liftEffect $ Console.log "Fetching inventory for edit..."
            result <- readInventory
            
            liftEffect case result of
              Right (InventoryData (Inventory items)) -> do
                Console.log $ "Found " <> show (length items) <> " items in inventory"
                
                case find (\(MenuItem item) -> show item.sku == actualUuid) items of
                  Just menuItem -> do
                    Console.log $ "Found item with UUID: " <> actualUuid
                    -- Create the EditItem component with the found item
                    currentRoute.push $ Tuple r (editItem menuItem)
                  Nothing -> do
                    Console.error $ "Item with UUID " <> actualUuid <> " not found"
                    errorState.push $ "Error: Item with UUID " <> actualUuid <> " not found"
                    currentRoute.push $ Tuple r (renderError $ "Item with UUID " <> actualUuid <> " not found")
              
              Right (Message msg) -> do
                Console.error $ "API error: " <> msg
                errorState.push $ "API error: " <> msg
                currentRoute.push $ Tuple r (renderError $ "API error: " <> msg)
              
              Left err -> do
                Console.error $ "Failed to fetch inventory: " <> err
                errorState.push $ "Failed to fetch inventory: " <> err
                currentRoute.push $ Tuple r (renderError $ "Failed to fetch inventory: " <> err)
            
            liftEffect $ loadingState.push false

  void $ matchesWith (parse route) matcher

  void $ runInBody
    ( fixed
        [ nav (fst <$> currentRoute.poll)
        , D.div_ [ cycle (snd <$> currentRoute.poll) ]
        ]
    )

  matcher Nothing LiveView