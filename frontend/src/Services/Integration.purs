module Inventory.Integration where

import Data.DateTime (DateTime(..), canonicalDate)
import Prelude
import Types.Inventory (ItemCategory, Species)
import Types.Transaction (Transaction, TransactionItem)

import Data.Array (length)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Finance.Currency (USD)
import Data.Finance.Money (Discrete(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Time (Time(..))
import Types.UUID (UUID)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

-- Using proper types rather than phantom types
type InventoryItem =
  { id :: UUID
  , productId :: UUID
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
  , vendor :: Maybe UUID
  , status :: InventoryStatus
  , location :: UUID
  , strain :: Maybe String
  , thcPercentage :: Maybe String
  , cbdPercentage :: Maybe String
  , species :: Maybe Species
  , isTestingCompleted :: Boolean
  , batchId :: Maybe String
  , properties :: InventoryProperties
  }

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

instance showInventoryStatus :: Show InventoryStatus where
  show Available = "Available"
  show OnHold = "On Hold"
  show Reserved = "Reserved"
  show Sold = "Sold"
  show Damaged = "Damaged"
  show Expired = "Expired"
  show InTransit = "In Transit"
  show UnderReview = "Under Review"
  show Recalled = "Recalled"

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

updateInventoryForTransaction ::
  Transaction ->
  Aff (Either InventoryError (Array InventoryUpdate))
updateInventoryForTransaction tx = do
  liftEffect $ log $ "Updating inventory for transaction: " <> show (unwrap tx).id
  pure $ Right []

checkInventoryAvailability ::
  Array TransactionItem ->
  Aff (Either InventoryError Boolean)
checkInventoryAvailability txnItems = do
  liftEffect $ log $ "Checking inventory availability for " <> show (length txnItems) <> " items"
  pure $ Right true

recordInventoryAdjustment ::
  UUID ->
  Number ->
  String ->
  UUID ->
  Aff (Either InventoryError InventoryAdjustment)
recordInventoryAdjustment itemId quantityChange reason employeeId = do
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

reconcileInventory ::
  UUID ->
  Array { inventoryItemId :: UUID, countedQuantity :: Number } ->
  UUID ->
  Aff (Either InventoryError InventoryReconciliation)
reconcileInventory locationId countItems employeeId = do
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

type InventoryAdjustment =
  { id :: String
  , inventoryItemId :: UUID
  , quantityChange :: Number
  , reason :: String
  , performedBy :: UUID
  , timestamp :: DateTime
  , notes :: Maybe String
  }

type InventoryReconciliation =
  { id :: String
  , locationId :: UUID
  , performedBy :: UUID
  , performedAt :: DateTime
  , items :: Array InventoryReconciliationItem
  , totalVariance :: Number
  , isApproved :: Boolean
  , approvedBy :: Maybe UUID
  }

type InventoryReconciliationItem =
  { inventoryItemId :: UUID
  , expectedQuantity :: Number
  , actualQuantity :: Number
  , variance :: Number
  , notes :: Maybe String
  }

type InventoryUpdate =
  { inventoryItemId :: UUID
  , previousQuantity :: Number
  , newQuantity :: Number
  , transactionId :: UUID
  , timestamp :: DateTime
  }

generateInventoryReport ::
  UUID ->
  Maybe ItemCategory ->
  Aff (Either InventoryError InventoryReport)
generateInventoryReport locationId catFilter = do
  liftEffect $ log $ "Generating inventory report for location " <> show locationId
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

type InventoryReport =
  { locationId :: UUID
  , generatedAt :: DateTime
  , totalItems :: Int
  , totalValue :: Discrete USD
  , itemsByCategory :: Array CategorySummary
  , lowStockItems :: Array InventoryItem
  }

type CategorySummary =
  { category :: ItemCategory
  , itemCount :: Int
  , totalValue :: Discrete USD
  }

-- Default datetime for testing/placeholder purposesdefaultDateTime :: DateTime
defaultDateTime :: DateTime
defaultDateTime =
  let
    -- Create the date component using toEnum to safely create Year, Month, and Day
    date = unsafePartial $ canonicalDate 
            (fromJust $ toEnum 2025)
            (fromJust $ toEnum 3) 
            (fromJust $ toEnum 5)
    
    -- Create time component
    time = Time 
            (unsafePartial $ fromJust $ toEnum 0)
            (unsafePartial $ fromJust $ toEnum 0)
            (unsafePartial $ fromJust $ toEnum 0)
            (unsafePartial $ fromJust $ toEnum 0)
  in 
    DateTime date time