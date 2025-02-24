module Main where

import Prelude

import API (fetchInventory)
import Control.Monad.ST.Class (liftST)
import CreateItem (createItem)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Deku.Core (Nut, fixed)
import Deku.DOM as D
import Deku.Hooks (cycle)
import Deku.Toplevel (runInBody)
import EditItem (editItem)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import FRP.Poll as Poll
import MenuLiveView (createMenuLiveView)
import Route (Route(..), nav, route)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Types (Inventory(..), InventoryResponse(..))
import Types.LiveViewConfig (defaultViewConfig)
import UUIDGen (genUUID)

testItemUUID :: String
testItemUUID = "56b0d1f7-fa3b-4cd4-9e58-79e4724295b0"

main :: Effect Unit
main = do
  Console.log "Application starting"

  -- Create state containers
  currentRoute <- liftST Poll.create
  inventoryState <- liftST Poll.create
  loadingState <- liftST Poll.create
  errorState <- liftST Poll.create
  
  -- Create a Poll for the UUID to pass to CreateItem
  currentUUID <- liftST Poll.create
  
  -- Generate an initial UUID
  initialUUID <- genUUID
  currentUUID.push (show initialUUID)
  
  -- Create the LiveView component with connected state
  let menuLiveView = createMenuLiveView inventoryState.poll loadingState.poll errorState.poll
  
  -- Map route to the appropriate component
  let routeToComponent = case _ of
        LiveView -> menuLiveView
        Create -> createItem currentUUID.poll  -- Pass the UUID Poll to CreateItem
        Edit uuid -> editItem (if uuid == "test" then testItemUUID else uuid)
  
  -- Define the route matcher function
  let
    matcher _ r = do
      Console.log $ "Route changed to: " <> show r
      
      -- Special handling for Create route - generate a new UUID
      when (r == Create) do
        newUUID <- genUUID
        currentUUID.push (show newUUID)
        Console.log $ "Generated new UUID for Create page: " <> show newUUID
      
      -- Update the route first (for responsive UI)
      currentRoute.push $ Tuple r (routeToComponent r)
      
      -- Load data based on the route
      case r of
        LiveView -> do
          -- Set loading state
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
                -- Update the inventory state - this will update the UI
                inventoryState.push inv
                loadingState.push false
              
              Right (Message msg) -> do
                Console.log $ "Received message: " <> msg
                loadingState.push false
                errorState.push msg
        
        _ -> pure unit

  -- Set up the hash-based router
  void $ matchesWith (parse route) matcher
  
  -- Mount the application with the combined navigation and page content
  void $ runInBody
    ( fixed
        [ nav (fst <$> currentRoute.poll)
        , D.div_ [ cycle (snd <$> currentRoute.poll) ]
        ]
    )

  -- Initialize with LiveView
  matcher Nothing LiveView