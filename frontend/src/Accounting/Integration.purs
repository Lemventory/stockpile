module Inventory.Integration where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array (filter)
import Data.Finance.Currency (USD)
import Data.Finance.Money (Discrete(..))
import Data.DateTime (DateTime)
import Data.UUID (UUID)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Types.Transaction
  ( Transaction
  , TransactionItem
  , ID
  , ItemCategory
  , Species
  )

-- | Inventory item record
type InventoryItem =
  { id :: ID InventoryItem
  , productId :: ID Product
  , sku :: String
  , name :: String
  , quantity :: Number
  , measureUnit :: String
  , perPackage :: String
  , costPerUnit :: Discrete USD
  , pricePerUnit :: Discrete USD
  , category :: ItemCategory
  , subcategory :: String
  , lotNumber :: Maybe String
  , expirationDate :: Maybe DateTime
  , receivedDate :: DateTime
  , vendor :: Maybe (ID Vendor)
  , status :: InventoryStatus
  , location :: ID Location
  , strain :: Maybe String
  , thcPercentage :: Maybe String
  , cbdPercentage :: Maybe String
  , species :: Maybe Species
  , isTestingCompleted :: Boolean
  , batchId :: Maybe String
  , properties :: InventoryProperties
  }

-- | Inventory item status
data InventoryStatus
  = Available
  | OnHold
  | Reserved
  | Sold
  | Damaged
  | Expired
  | InTransit
  | UnderReview
  | Recalled

derive instance eqInventoryStatus :: Eq InventoryStatus
derive instance ordInventoryStatus :: Ord InventoryStatus

-- | Inventory properties for cannabis products
type InventoryProperties =
  { isPrepackaged :: Boolean
  , terpenes :: Array { name :: String, percentage :: Maybe String }
  , harvested :: Maybe DateTime
  , cured :: Maybe DateTime
  , harvestBatch :: Maybe String
  , testingLabId :: Maybe String
  , testResults :: Maybe TestResults
  , isWeighed :: Boolean
  , weightLoss :: Maybe Number
  }

-- | Laboratory test results
type TestResults =
  { thcTested :: Maybe Number
  , cbdTested :: Maybe Number
  , terpenesTested :: Maybe (Array { name :: String, percentage :: Number })
  , microbialPassed :: Boolean
  , pesticidePassed :: Boolean
  , heavyMetalsPassed :: Boolean
  , residualSolventsPassed :: Boolean
  , overallPassed :: Boolean
  , testedOn :: DateTime
  , expiresOn :: DateTime
  , labName :: String
  , labId :: String
  , certificateUrl :: Maybe String
  }

-- | Update inventory based on transaction
updateInventoryForTransaction :: 
  Transaction -> 
  Aff (Either InventoryError (Array InventoryUpdate))
updateInventoryForTransaction txn = do
  -- In a real system, this would:
  -- 1. Load inventory items based on transaction items
  -- 2. Reduce inventory quantities
  -- 3. Update status of sold items
  -- 4. Record the inventory changes in the system
  -- 5. Generate compliance reports if needed
  
  liftEffect $ log $ "Updating inventory for transaction: " <> show txn.id
  
  -- Placeholder implementation
  pure $ Right []

-- | Check if inventory is available for a transaction
checkInventoryAvailability :: 
  Array TransactionItem -> 
  Aff (Either InventoryError Boolean)
checkInventoryAvailability txnItems = do
  -- This would verify each item has sufficient quantity in inventory
  liftEffect $ log $ "Checking inventory availability for " <> show (length txnItems) <> " items"
  pure $ Right true

-- | Record inventory adjustment
recordInventoryAdjustment :: 
  ID InventoryItem -> 
  Number -> -- Quantity change (positive for increase, negative for decrease)
  String -> -- Reason
  ID Employee -> -- Performed by
  Aff (Either InventoryError InventoryAdjustment)
recordInventoryAdjustment itemId quantityChange reason employeeId = do
  -- This would document inventory adjustments for auditing
  liftEffect $ log $ "Recording inventory adjustment for item " <> show itemId
  pure $ Right {
    id: "adj-123",
    inventoryItemId: itemId,
    quantityChange,
    reason,
    performedBy: employeeId,
    timestamp: defaultDateTime,
    notes: Nothing
  }

-- | Run physical inventory reconciliation
reconcileInventory :: 
  ID Location -> 
  Array { inventoryItemId :: ID InventoryItem, countedQuantity :: Number } ->
  ID Employee ->
  Aff (Either InventoryError InventoryReconciliation)
reconcileInventory locationId countItems employeeId = do
  -- This would compare expected vs. actual quantities and calculate variances
  liftEffect $ log $ "Reconciling inventory at location " <> show locationId
  pure $ Right {
    id: "recon-123",
    locationId,
    performedBy: employeeId,
    performedAt: defaultDateTime,
    items: [],
    totalVariance: 0.0,
    isApproved: false,
    approvedBy: Nothing
  }

-- | Inventory errors
data InventoryError
  = InsufficientStock
  | ItemNotFound
  | InvalidQuantity
  | InvalidLot
  | ExpiredProduct
  | PermissionDenied
  | ComplianceError String
  | DatabaseError String

derive instance eqInventoryError :: Eq InventoryError
derive instance ordInventoryError :: Ord InventoryError

instance showInventoryError :: Show InventoryError where
  show InsufficientStock = "Insufficient inventory stock"
  show ItemNotFound = "Item not found in inventory"
  show InvalidQuantity = "Invalid quantity specified"
  show InvalidLot = "Invalid lot number"
  show ExpiredProduct = "Product has expired"
  show PermissionDenied = "Permission denied for inventory operation"
  show (ComplianceError msg) = "Compliance error: " <> msg
  show (DatabaseError msg) = "Database error: " <> msg

-- | Inventory adjustment record
type InventoryAdjustment =
  { id :: String
  , inventoryItemId :: ID InventoryItem
  , quantityChange :: Number
  , reason :: String
  , performedBy :: ID Employee
  , timestamp :: DateTime
  , notes :: Maybe String
  }

-- | Inventory reconciliation record
type InventoryReconciliation =
  { id :: String
  , locationId :: ID Location
  , performedBy :: ID Employee
  , performedAt :: DateTime
  , items :: Array InventoryReconciliationItem
  , totalVariance :: Number
  , isApproved :: Boolean
  , approvedBy :: Maybe (ID Employee)
  }

-- | Inventory reconciliation item
type InventoryReconciliationItem =
  { inventoryItemId :: ID InventoryItem
  , expectedQuantity :: Number
  , actualQuantity :: Number
  , variance :: Number
  , notes :: Maybe String
  }

-- | Inventory update record
type InventoryUpdate =
  { inventoryItemId :: ID InventoryItem
  , previousQuantity :: Number
  , newQuantity :: Number
  , transactionId :: ID Transaction
  , timestamp :: DateTime
  }

-- | Generate inventory report by category
generateInventoryReport :: 
  ID Location -> 
  Maybe ItemCategory ->
  Aff (Either InventoryError InventoryReport)
generateInventoryReport locationId catFilter = do
  -- This would generate reports on current inventory levels
  liftEffect $ log $ "Generating inventory report for location " <> show locationId
  
  -- If category filter is specified, log it
  case catFilter of
    Just cat -> liftEffect $ log $ "Filtering by category: " <> show cat
    Nothing -> liftEffect $ log "No category filter applied"
    
  pure $ Right {
    locationId,
    generatedAt: defaultDateTime,
    totalItems: 0,
    totalValue: Discrete 0,
    itemsByCategory: [],
    lowStockItems: []
  }

-- | Inventory report
type InventoryReport =
  { locationId :: ID Location
  , generatedAt :: DateTime
  , totalItems :: Int
  , totalValue :: Discrete USD
  , itemsByCategory :: Array CategorySummary
  , lowStockItems :: Array InventoryItem
  }

-- | Category summary for inventory report
type CategorySummary =
  { category :: ItemCategory
  , itemCount :: Int
  , totalValue :: Discrete USD
  }

-- Helper for array length (to avoid foreign import)
length :: forall a. Array a -> Int
length [] = 0
length (_ : xs) = 1 + length xs

-- Placeholder for external types
type Product = {}
type Employee = {}
type Location = {}
type Vendor = {}

defaultDateTime :: DateTime
defaultDateTime = unsafeCoerce "2025-03-05T00:00:00.000Z"

-- | Unsafe coerce for example purposes only
foreign import unsafeCoerce :: forall a b. a -> b