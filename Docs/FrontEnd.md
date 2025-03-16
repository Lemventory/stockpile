# stockpile Frontend Documentation

## Table of Contents
- [Overview](#overview)
- [Technologies](#technologies)
- [Architecture](#architecture)
- [Components](#components)
- [Routing](#routing)
- [State Management](#state-management)
- [API Integration](#api-integration)
- [Data Models](#data-models)
- [Form Handling](#form-handling)
- [Validation](#validation)
- [Transaction Processing](#transaction-processing)
- [Services](#services)
- [Utilities](#utilities)
- [Development Guidelines](#development-guidelines)

## Overview

stockpile is a comprehensive dispensary management system built with PureScript. The frontend provides a user-friendly interface for inventory management, point-of-sale operations, transaction processing, and compliance tracking. The application connects to a backend API to perform CRUD operations and business logic processing.

## Technologies

The frontend utilizes modern PureScript libraries and technologies:

- **Deku**: A declarative UI library for PureScript with hooks-like functionality for component development
- **FRP**: Functional Reactive Programming principles for state management through the Poll mechanism
- **Routing**: Client-side routing using hash-based navigation with Routing.Duplex and Routing.Hash
- **Fetch**: HTTP client for making requests to the backend API
- **Yoga.JSON**: Data serialization and deserialization for API communication
- **Data.Validation.Semigroup**: Form validation with semigroup-based validation
- **Data.Finance.Money**: Precise handling of monetary values with Discrete USD types for financial calculations
- **Effect.Aff**: Asynchronous effect handling for API calls and side effects

## Architecture

The application follows a component-based architecture pattern with clear separation of concerns:

1. **Main Module**: Entry point that sets up routing, initializes global state, and renders the application
2. **Components**: Reusable UI components for different views and functionality
3. **API Modules**: Separate modules for Inventory and Transaction API communication
4. **Types**: Shared domain models and type definitions with proper newtypes and type classes
5. **Services**: Business logic services for Cash Register, Ledger, Compliance, and Inventory management
6. **Validation**: Form validation logic with composable validation rules
7. **Utilities**: Helper functions for formatting, UUID generation, money handling, and validation

## Components

### Inventory Management

#### MenuLiveView
Displays inventory items in a grid layout with:
- Real-time updates and refresh capabilities
- Status indicators for loading and errors
- Customizable appearance based on product categories and species
- Links to edit and delete items

```purescript
createMenuLiveView :: Poll Inventory -> Poll Boolean -> Poll String -> Nut
createMenuLiveView inventoryPoll loadingPoll errorPoll = ...
```

#### CreateItem
Form component for adding new inventory items with:
- Comprehensive validation for all fields
- Status messages and error handling
- Reset functionality
- Debug information panel

```purescript
createItem :: String -> Nut
createItem initialUUID = Deku.do
  -- State hooks for form fields
  setSku /\ skuValue <- useState initialUUID
  setName /\ nameValue <- useState ""
  -- Additional form fields...
```

#### EditItem
Component for modifying existing inventory items:
- Pre-populated form fields from existing item data
- Real-time validation
- Status messages for submission results
- Error handling for failed API calls

```purescript
editItem :: MenuItem -> Nut
editItem (MenuItem item) = Deku.do
  let (StrainLineage lineage) = item.strain_lineage
  -- Form state management...
```

#### DeleteItem
Confirmation component for removing inventory items:
- Warning about irreversible action
- Cancellation option
- Success and error state handling

```purescript
deleteItem :: String -> String -> Nut
deleteItem itemId itemName = Deku.do
  setStatusMessage /\ statusMessageEvent <- useState ""
  setSubmitting /\ submittingEvent <- useState false
  -- Delete confirmation UI...
```

### Transaction Processing

#### CreateTransaction
A comprehensive POS interface that allows:
- Adding items from inventory to the transaction
- Specifying quantities and calculating subtotals
- Computing taxes automatically
- Processing various payment methods (Cash, Credit, Debit, etc.)
- Finalizing sales transactions
- Handling change calculation for cash payments

```purescript
createTransaction :: Nut
createTransaction = Deku.do
  -- Transaction state
  setItems /\ itemsValue <- useState []
  setPayments /\ paymentsValue <- useState []
  -- Additional transaction management...
```

#### TransactionHistory
Displays historical transactions with:
- Sortable and filterable transaction list
- Detailed view of transaction line items
- Transaction status indicators
- Payment details
- Summary calculations
- Search functionality

```purescript
transactionHistory :: Nut
transactionHistory = Deku.do
  setTransactions /\ transactionsValue <- useState []
  setSelectedTransaction /\ selectedTransactionValue <- useState Nothing
  -- Transaction history view...
```

## Routing

The application uses hash-based routing with routes defined in the `Route` module:

```purescript
data Route = LiveView | Create | Edit String | Delete String

route :: RouteDuplex' Route
route = root $ G.sum
  { "LiveView": G.noArgs
  , "Create": "create" / G.noArgs
  , "Edit": "edit" / (string segment)
  , "Delete": "delete" / (string segment)
  }
```

The routing system:
- Handles navigation between views
- Parses URL parameters
- Updates the current route in the application state
- Loads appropriate data based on route changes
- Renders the corresponding component

## State Management

The application employs Functional Reactive Programming (FRP) principles for state management:

- **Global State**: Managed through Poll mechanisms in the Main module for application-wide concerns
- **Component State**: Local state using Deku's useState hooks for component-specific data
- **Derived State**: Computed values from base state using FRP operators
- **Shared State**: Important state like inventory data is shared across relevant components
- **Async State**: Loading and error states for async operations like API calls

```purescript
-- Example of state hooks in Main.purs
currentRoute <- liftST Poll.create
inventoryState <- liftST Poll.create
loadingState <- liftST Poll.create
errorState <- liftST Poll.create
```

## API Integration

The application includes separate API modules for different domains:

### Inventory API (`API.Inventory`)
- `readInventory`: Fetches all inventory items
- `writeInventory`: Creates a new inventory item
- `updateInventory`: Updates an existing inventory item
- `deleteInventory`: Removes an inventory item
- `fetchInventory`: Flexible inventory fetching with configuration options
- `fetchInventoryFromJson`: Retrieves inventory from JSON files
- `fetchInventoryFromHttp`: Fetches inventory from the HTTP backend

### Transaction API (`API.Transaction`)
- `createTransaction`: Creates a new transaction
- `addTransactionItem`: Adds an item to a transaction
- `removeTransactionItem`: Removes an item from a transaction
- `addPaymentTransaction`: Records a payment for a transaction
- `removePaymentTransaction`: Removes a payment from a transaction
- `finalizeTransaction`: Completes a transaction
- `getTransaction`: Retrieves transaction details
- `getAllTransactions`: Gets transaction history
- `voidTransaction`: Voids a transaction
- `refundTransaction`: Processes a refund

All API functions use the Effect.Aff monad for asynchronous operations and return Either types for error handling.

## Data Models

### Inventory Models

#### MenuItem
The core product data structure with comprehensive properties:
```purescript
newtype MenuItem = MenuItem MenuItemRecord

type MenuItemRecord =
  { sort :: Int
  , sku :: UUID
  , brand :: String
  , name :: String
  , price :: Discrete USD
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
```

#### ItemCategory
Product category enumeration:
```purescript
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
```

#### StrainLineage
Cannabis strain information:
```purescript
data StrainLineage = StrainLineage
  { thc :: String
  , cbg :: String
  , strain :: String
  , creator :: String
  , species :: Species
  , dominant_terpene :: String
  , terpenes :: Array String
  , lineage :: Array String
  , leafly_url :: String
  , img :: String
  }
```

#### Species
Cannabis species classification:
```purescript
data Species
  = Indica
  | IndicaDominantHybrid
  | Hybrid
  | SativaDominantHybrid
  | Sativa
```

### Transaction Models

#### Transaction
Core transaction data:
```purescript
newtype Transaction = Transaction
  { id :: UUID
  , status :: TransactionStatus
  , created :: DateTime
  , completed :: Maybe DateTime
  , customer :: Maybe UUID
  , employee :: UUID
  , register :: UUID
  , location :: UUID
  , items :: Array TransactionItem
  , payments :: Array PaymentTransaction
  , subtotal :: DiscreteMoney USD
  , discountTotal :: DiscreteMoney USD
  , taxTotal :: DiscreteMoney USD
  , total :: DiscreteMoney USD
  , transactionType :: TransactionType
  , isVoided :: Boolean
  , voidReason :: Maybe String
  , isRefunded :: Boolean
  , refundReason :: Maybe String
  , referenceTransactionId :: Maybe UUID
  , notes :: Maybe String
  }
```

#### TransactionItem
Line items within a transaction:
```purescript
newtype TransactionItem = TransactionItem
  { id :: UUID
  , transactionId :: UUID
  , menuItemSku :: UUID
  , quantity :: Number
  , pricePerUnit :: DiscreteMoney USD
  , discounts :: Array DiscountRecord
  , taxes :: Array TaxRecord
  , subtotal :: DiscreteMoney USD
  , total :: DiscreteMoney USD
  }
```

#### PaymentTransaction
Payment details:
```purescript
newtype PaymentTransaction = PaymentTransaction
  { id :: UUID
  , transactionId :: UUID
  , method :: PaymentMethod
  , amount :: DiscreteMoney USD
  , tendered :: DiscreteMoney USD
  , change :: DiscreteMoney USD
  , reference :: Maybe String
  , approved :: Boolean
  , authorizationCode :: Maybe String
  }
```

#### TransactionStatus
Status enumeration:
```purescript
data TransactionStatus
  = Created
  | InProgress
  | Completed
  | Voided
  | Refunded
```

#### PaymentMethod
Payment type enumeration:
```purescript
data PaymentMethod
  = Cash
  | Debit
  | Credit
  | ACH
  | GiftCard
  | StoredValue
  | Mixed
  | Other String
```

#### Financial Records
Tax and discount management:
```purescript
type TaxRecord =
  { category :: TaxCategory
  , rate :: Number
  , amount :: DiscreteMoney USD
  , description :: String
  }

type DiscountRecord =
  { type :: DiscountType
  , amount :: DiscreteMoney USD
  , reason :: String
  , approvedBy :: Maybe UUID
  }
```

## Form Handling

The application implements a robust form handling system:

### Form Field Components
Reusable field components in the `UI.Common.Form` module:
```purescript
makeTextField :: FieldConfig -> (String -> Effect Unit) -> (Maybe Boolean -> Effect Unit) -> Poll (Maybe Boolean) -> Boolean -> Nut
makeDropdown :: DropdownConfig -> (String -> Effect Unit) -> (Maybe Boolean -> Effect Unit) -> Poll (Maybe Boolean) -> Nut
makeDescriptionField :: String -> (String -> Effect Unit) -> (Maybe Boolean -> Effect Unit) -> Poll (Maybe Boolean) -> Nut
```

### Field Configuration
Consistent configuration objects in the `Config.InventoryFields` module:
```purescript
nameConfig :: String -> FieldConfig
nameConfig defaultValue =
  { label: "Name"
  , placeholder: "Enter product name"
  , defaultValue
  , validation: allOf [ nonEmpty, extendedAlphanumeric, maxLength 50 ]
  , errorMessage: "Name is required and must be less than 50 characters"
  , formatInput: trim
  }
```

### Form State Management
Each form manages:
- Field values with useState hooks
- Validation state with separate state hooks
- Form-level validity computation
- Submission status and error handling
- Reset functionality

### Form Submission Process
1. Collect form values from state
2. Validate the complete form
3. Handle validation errors
4. Format values for API submission
5. Make API call using Effect.Aff
6. Handle success and error responses
7. Update UI with appropriate feedback

## Validation

The application includes a comprehensive validation system:

### Validation Rules
Basic validation rules in the `Utils.Validation` module:
```purescript
nonEmpty :: ValidationRule
alphanumeric :: ValidationRule
extendedAlphanumeric :: ValidationRule
percentage :: ValidationRule
dollarAmount :: ValidationRule
validMeasurementUnit :: ValidationRule
validUrl :: ValidationRule
positiveInteger :: ValidationRule
nonNegativeInteger :: ValidationRule
fraction :: ValidationRule
commaList :: ValidationRule
validUUID :: ValidationRule
maxLength :: Int -> ValidationRule
```

### Rule Composition
Composable validation with combinators:
```purescript
allOf :: Array ValidationRule -> ValidationRule
anyOf :: Array ValidationRule -> ValidationRule
```

### Validation Architecture
- `ValidationRule` newtype for encapsulating validation functions
- Semigroup-based error collection
- Either-based validation results
- Real-time validation with immediate feedback
- Comprehensive field-level and form-level validation

### Complex Form Validation
Complete form validation for MenuItem creation/updates:
```purescript
validateMenuItem :: MenuItemFormInput -> Either String MenuItem
validateStrainLineage :: StrainLineageFormInput -> V (Array String) StrainLineage
```

## Transaction Processing

The frontend implements the complete transaction lifecycle:

### Transaction Creation
1. Initialize a new transaction with employee, register, and location IDs
2. Add items from inventory with quantities
3. Apply taxes automatically based on product categories
4. Calculate subtotals, tax totals, and grand totals

### Payment Processing
1. Record payments with different payment methods
2. Handle cash payments with tendered amount and change calculation
3. Support multiple payments for a single transaction
4. Track payment totals and remaining balance

### Transaction Finalization
1. Validate transaction completeness
2. Ensure sufficient payment
3. Create a complete transaction record
4. Submit to the backend API
5. Handle success and error cases

### Receipt Generation
Support for receipt creation with formatTransactionItem and generateReceipt functions.

## Services

### CashRegister Service
Handles cash drawer operations and transaction processing:

```purescript
type RegisterState =
  { isOpen :: Boolean
  , currentDrawerAmount :: Discrete USD
  , currentTransaction :: Maybe Transaction
  , openedAt :: Maybe DateTime
  , openedBy :: Maybe UUID
  , lastTransactionTime :: Maybe DateTime
  , expectedDrawerAmount :: Discrete USD
  }

openRegister :: UUID -> UUID -> Discrete USD -> Aff (Either RegisterError RegisterState)
closeRegister :: RegisterState -> UUID -> Discrete USD -> Aff (Either RegisterError { closingState :: RegisterState, variance :: Discrete USD })
```

### Ledger Service
Manages financial accounting operations:

```purescript
createSaleEntries :: Transaction -> UUID -> UUID -> UUID -> Aff (Either LedgerError (Array LedgerEntry))
createRefundEntries :: Transaction -> UUID -> UUID -> UUID -> Aff (Either LedgerError (Array LedgerEntry))
getAccountBalance :: Array LedgerEntry -> UUID -> Maybe DateTime -> Discrete USD
calculateDailySales :: Array LedgerEntry -> DateTime -> DateTime -> Aff { cash :: Discrete USD, card :: Discrete USD, other :: Discrete USD, total :: Discrete USD }
```

### Compliance Service
Handles regulatory compliance requirements:

```purescript
checkCustomerEligibility :: UUID -> Maybe String -> Maybe Boolean -> UUID -> Aff (Either ComplianceError (Array CustomerVerification))
checkPurchaseLimits :: UUID -> Array TransactionItem -> Array Transaction -> Aff (Either ComplianceError Boolean)
createComplianceRecord :: Transaction -> Array CustomerVerification -> Aff (Either ComplianceError ComplianceRecord)
submitToStateTracking :: Transaction -> ComplianceRecord -> Aff (Either ComplianceError { updatedRecord :: ComplianceRecord, referenceId :: String })
```

### Inventory Integration Service
Manages inventory operations:

```purescript
updateInventoryForTransaction :: Transaction -> Aff (Either InventoryError (Array InventoryUpdate))
checkInventoryAvailability :: Array TransactionItem -> Aff (Either InventoryError Boolean)
recordInventoryAdjustment :: UUID -> Number -> String -> UUID -> Aff (Either InventoryError InventoryAdjustment)
reconcileInventory :: UUID -> Array { inventoryItemId :: UUID, countedQuantity :: Number } -> UUID -> Aff (Either InventoryError InventoryReconciliation)
```

## Utilities

### Formatting Utilities
Helper functions for formatting and display:
```purescript
uuidToString :: UUID -> String
generateClassName :: { category :: ItemCategory, subcategory :: String, species :: Species } -> String
toClassName :: String -> String
randomInt :: Int -> Int -> Effect Int
ensureNumber :: String -> String
ensureInt :: String -> String
summarizeLongText :: String -> String
```

### Money Utilities
Precise money handling functions:
```purescript
fromDollars :: Number -> Discrete USD
toDollars :: Discrete USD -> Number
formatMoney :: DiscreteMoney USD -> String
formatMoney' :: DiscreteMoney USD -> String
parseMoneyString :: String -> Maybe (Discrete USD)
```

### UUID Generation
UUID creation and handling:
```purescript
genUUID :: Effect UUID
parseUUID :: String -> Maybe UUID
```

## Development Guidelines

### Project Structure
The codebase is organized into logical modules:
- `/src/API/` - API communication modules
- `/src/Config/` - Configuration constants and settings
- `/src/Services/` - Business logic services
- `/src/Types/` - Data models and type definitions
- `/src/UI/` - UI components organized by domain
- `/src/Utils/` - Utility functions and helpers

### Error Handling
The application implements comprehensive error handling:
- API errors with meaningful messages
- Form validation errors with specific field feedback
- Business logic errors with appropriate user messages
- Network and system error handling
- Error state management in the UI

### Performance Considerations
- Efficient state updates with minimized re-renders
- Lazy loading of components
- Proper cleanup of resources

### Type Safety
- Strong typing throughout with newtypes and sum types
- Instance implementations for serialization
- Validation of all user inputs

### Future Development
Areas for potential enhancement:
- Integration with hardware peripherals (receipt printers, barcode scanners)
- Advanced reporting and analytics
- Customer loyalty and rewards programs
- Inventory forecasting and automated ordering
- Enhanced compliance tracking for regulatory requirements