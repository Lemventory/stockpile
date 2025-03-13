module UI.Common.StateManager where

import Prelude

import Control.Monad.ST.Class (liftST)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Poll (Poll, create)
import Services.API (class MonadService, runLiveService)
import Types.Common (ServiceError)
import Types.Inventory (Inventory, MenuItem)
import Types.Transaction (Transaction)

-- | State container for the entire application
type AppState = {
  inventory :: Poll Inventory,
  selectedItem :: Poll (Maybe MenuItem),
  currentTransaction :: Poll (Maybe Transaction),
  loading :: Poll Boolean,
  error :: Poll (Maybe ServiceError)
}

-- | Initialize the application state
initAppState :: Aff AppState
initAppState = do
  -- Use liftST to lift ST actions into Aff
  inventoryPoll <- liftEffect $ liftST create
  selectedItemPoll <- liftEffect $ liftST create
  transactionPoll <- liftEffect $ liftST create
  errorPoll <- liftEffect $ liftST create
  loadingPoll <- liftEffect $ liftST create
  
  -- Return the state
  pure
    { inventory: inventoryPoll.poll
    , selectedItem: selectedItemPoll.poll
    , currentTransaction: transactionPoll.poll
    , error: errorPoll.poll
    , loading: loadingPoll.poll
    }

-- | Actions that can be performed on the state
data AppAction
  = LoadInventory
  | SelectItem MenuItem
  | ClearSelectedItem
  | StartTransaction
  | AddItemToTransaction MenuItem Int
  | CompleteTransaction
  | SetError ServiceError
  | ClearError

-- | Reducer function to handle actions
reduceAction :: forall m. MonadService m => MonadEffect m => AppState -> AppAction -> m AppState
reduceAction state action = case action of
  LoadInventory -> do
    -- Load inventory and update state
    pure state
    
  SelectItem item -> do
    -- Update selected item
    pure state
    
  ClearSelectedItem -> do
    -- Clear selected item
    pure state
    
  StartTransaction -> do
    -- Initialize a new transaction
    pure state
    
  AddItemToTransaction item quantity -> do
    -- Add item to current transaction
    pure state
    
  CompleteTransaction -> do
    -- Finalize the current transaction
    pure state
    
  SetError err -> do
    -- Set error state
    pure state
    
  ClearError -> do
    -- Clear error state
    pure state

-- | Helper to dispatch an action with a live service
dispatchAction :: AppState -> AppAction -> Aff AppState
dispatchAction state action = runLiveService (reduceAction state action)