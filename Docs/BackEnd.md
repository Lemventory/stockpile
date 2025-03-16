# stockpile Backend Documentation

## Table of Contents

- [Overview](#overview)
- [Architecture](#architecture)
- [Core Components](#core-components)
- [API Reference](#api-reference)
- [Data Models](#data-models)
- [Database Schema](#database-schema)
- [Transaction Processing](#transaction-processing)
- [Security and Configuration](#security-and-configuration)
- [Development Guidelines](#development-guidelines)

## Overview

The stockpile backend is a Haskell-based API server built for inventory and transaction management in retail operations, with a focus on cannabis dispensary requirements. It provides comprehensive functionality for inventory tracking, point-of-sale operations, sales transactions, and compliance reporting.

The system is designed with a layered architecture following functional programming principles and leverages PostgreSQL for data persistence. It supports real-time inventory management, transaction processing, cash register operations, and financial reporting.

## Architecture

The backend follows a clean, layered architecture pattern:

### Architectural Layers

1. **API Layer**: Defines the RESTful API endpoints using Servant's type-level DSL
2. **Server Layer**: Implements the API handlers and routes requests
3. **Database Layer**: Manages database interactions and provides CRUD operations
4. **Application Core**: Configures and bootstraps the application
5. **Types Layer**: Defines the domain models and data types

### Key Technologies

- **Servant**: Type-level web API definition and server implementation
- **PostgreSQL**: Primary database with `postgresql-simple` library
- **Warp**: High-performance HTTP server
- **Resource Pooling**: Connection management via `resource-pool`
- **CORS**: Cross-Origin Resource Sharing support for web integration

### System Flow

1. Client requests are received by the Warp server
2. Servant routes requests to the appropriate handler
3. Handlers process business logic and interact with the database layer
4. Database layer manages PostgreSQL connections and executes queries
5. Results are transformed back into API responses
6. Responses are sent back to the client

## Core Components

### Main Application (`App.hs`)

The application entry point handles server configuration, database initialization, and middleware setup:

```haskell
run :: IO ()
run = do
  currentUser <- getLoginName
  let config =
        AppConfig
          { dbConfig =
              DBConfig
                { dbHost = "localhost"
                , dbPort = 5432
                , dbName = "stockpile"
                , dbUser = currentUser
                , dbPassword = "postgres"
                , poolSize = 10
                }
          , serverPort = 8080
          }

  pool <- initializeDB (dbConfig config)
  createTables pool
  createTransactionTables pool
  
  putStrLn $ "Starting server on all interfaces, port " ++ show (serverPort config)
  putStrLn "=================================="
  putStrLn $ "Server running on port " ++ show (serverPort config)
  putStrLn "You can access this application from other devices on your network using:"
  putStrLn $ "http://YOUR_MACHINE_IP:" ++ show (serverPort config)
  putStrLn "=================================="

  -- Configure CORS and start server
  let corsPolicy = CorsResourcePolicy {...}
  let app = cors (const $ Just corsPolicy) $ serve api (combinedServer pool)
  
  Warp.run (serverPort config) app
```

### API Definition (`API/Inventory.hs` and `API/Transaction.hs`)

The API is defined using Servant's type-level DSL:

#### Inventory API

```haskell
type InventoryAPI =
  "inventory" :> Get '[JSON] InventoryResponse
    :<|> "inventory" :> ReqBody '[JSON] MenuItem :> Post '[JSON] InventoryResponse
    :<|> "inventory" :> ReqBody '[JSON] MenuItem :> Put '[JSON] InventoryResponse
    :<|> "inventory" :> Capture "sku" UUID :> Delete '[JSON] InventoryResponse
```

#### Transaction API

Provides comprehensive endpoints for transaction management:

```haskell
type TransactionAPI =
  "transaction" :> Get '[JSON] [Transaction]
    :<|> "transaction" :> Capture "id" UUID :> Get '[JSON] Transaction
    :<|> "transaction" :> ReqBody '[JSON] Transaction :> Post '[JSON] Transaction
    :<|> "transaction" :> Capture "id" UUID :> ReqBody '[JSON] Transaction :> Put '[JSON] Transaction
    :<|> "transaction" :> "void" :> Capture "id" UUID :> ReqBody '[JSON] Text :> Post '[JSON] Transaction
    :<|> "transaction" :> "refund" :> Capture "id" UUID :> ReqBody '[JSON] Text :> Post '[JSON] Transaction
    :<|> "transaction" :> "item" :> ReqBody '[JSON] TransactionItem :> Post '[JSON] TransactionItem
    :<|> "transaction" :> "item" :> Capture "id" UUID :> Delete '[JSON] NoContent
    :<|> "transaction" :> "payment" :> ReqBody '[JSON] PaymentTransaction :> Post '[JSON] PaymentTransaction
    :<|> "transaction" :> "payment" :> Capture "id" UUID :> Delete '[JSON] NoContent
    :<|> "transaction" :> "finalize" :> Capture "id" UUID :> Post '[JSON] Transaction
```

#### Register API

Handles operations related to cash registers:

```haskell
type RegisterAPI =
  "register" :> Get '[JSON] [Register]
    :<|> "register" :> Capture "id" UUID :> Get '[JSON] Register
    :<|> "register" :> ReqBody '[JSON] Register :> Post '[JSON] Register
    :<|> "register" :> Capture "id" UUID :> ReqBody '[JSON] Register :> Put '[JSON] Register
    :<|> "register" :> "open" :> Capture "id" UUID :> ReqBody '[JSON] OpenRegisterRequest :> Post '[JSON] Register
    :<|> "register" :> "close" :> Capture "id" UUID :> ReqBody '[JSON] CloseRegisterRequest :> Post '[JSON] CloseRegisterResult
```

### Database Layer (`DB/Database.hs` and `DB/Transaction.hs`)

Manages database connections and operations:

- Connection pooling for efficient resource management
- Retry logic with exponential backoff for connection failures
- Prepared statements for query execution
- Transaction safety

Key database functions include:

```haskell
initializeDB :: DBConfig -> IO (Pool.Pool Connection)
createTables :: Pool.Pool Connection -> IO ()
createTransactionTables :: ConnectionPool -> IO ()
getAllMenuItems :: Pool.Pool Connection -> IO Inventory
insertMenuItem :: Pool.Pool Connection -> MenuItem -> IO ()
updateExistingMenuItem :: Pool.Pool Connection -> MenuItem -> IO ()
deleteMenuItem :: Pool.Pool Connection -> UUID -> Handler InventoryResponse
```

## API Reference

### Inventory Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/inventory` | Retrieve all inventory items |
| POST | `/inventory` | Add a new inventory item |
| PUT | `/inventory` | Update an existing inventory item |
| DELETE | `/inventory/:sku` | Delete an inventory item by SKU |

### Transaction Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/transaction` | Get all transactions |
| GET | `/transaction/:id` | Get a transaction by ID |
| POST | `/transaction` | Create a new transaction |
| PUT | `/transaction/:id` | Update a transaction |
| POST | `/transaction/void/:id` | Void a transaction |
| POST | `/transaction/refund/:id` | Refund a transaction |
| POST | `/transaction/item` | Add an item to a transaction |
| DELETE | `/transaction/item/:id` | Remove an item from a transaction |
| POST | `/transaction/payment` | Add a payment to a transaction |
| DELETE | `/transaction/payment/:id` | Remove a payment from a transaction |
| POST | `/transaction/finalize/:id` | Finalize a transaction |

### Register Management Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/register` | Get all registers |
| GET | `/register/:id` | Get register by ID |
| POST | `/register` | Create a register |
| PUT | `/register/:id` | Update a register |
| POST | `/register/open/:id` | Open a register |
| POST | `/register/close/:id` | Close a register |

### Ledger and Compliance Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/ledger/entry` | Get all ledger entries |
| GET | `/ledger/account` | Get all accounts |
| POST | `/ledger/report/daily` | Generate daily report |
| POST | `/compliance/verification` | Verify customer compliance |
| GET | `/compliance/record/:transaction_id` | Get compliance record |
| POST | `/compliance/report` | Generate compliance report |

## Data Models

### Inventory Models

The core inventory data model consists of:

#### MenuItem

Represents a product in the inventory:

```haskell
data MenuItem = MenuItem
  { sort :: Int
  , sku :: UUID
  , brand :: Text
  , name :: Text
  , price :: Int  -- Stored as cents
  , measure_unit :: Text
  , per_package :: Text
  , quantity :: Int
  , category :: ItemCategory
  , subcategory :: Text
  , description :: Text
  , tags :: V.Vector Text
  , effects :: V.Vector Text
  , strain_lineage :: StrainLineage
  }
```

#### ItemCategory

```haskell
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
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, Read)
```

#### StrainLineage

Contains detailed information for cannabis products:

```haskell
data StrainLineage = StrainLineage
  { thc :: Text
  , cbg :: Text
  , strain :: Text
  , creator :: Text
  , species :: Species
  , dominant_terpene :: Text
  , terpenes :: V.Vector Text
  , lineage :: V.Vector Text
  , leafly_url :: Text
  , img :: Text
  }
```

#### Species

```haskell
data Species
  = Indica
  | IndicaDominantHybrid
  | Hybrid
  | SativaDominantHybrid
  | Sativa
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, Read)
```

### Transaction Models

#### Transaction

The core transaction model:

```haskell
data Transaction = Transaction
  { transactionId :: UUID
  , transactionStatus :: TransactionStatus
  , transactionCreated :: UTCTime
  , transactionCompleted :: Maybe UTCTime
  , transactionCustomerId :: Maybe UUID
  , transactionEmployeeId :: UUID
  , transactionRegisterId :: UUID
  , transactionLocationId :: UUID
  , transactionItems :: [TransactionItem]
  , transactionPayments :: [PaymentTransaction]
  , transactionSubtotal :: Int
  , transactionDiscountTotal :: Int
  , transactionTaxTotal :: Int
  , transactionTotal :: Int
  , transactionType :: TransactionType
  , transactionIsVoided :: Bool
  , transactionVoidReason :: Maybe Text
  , transactionIsRefunded :: Bool
  , transactionRefundReason :: Maybe Text
  , transactionReferenceTransactionId :: Maybe UUID
  , transactionNotes :: Maybe Text
  }
```

#### TransactionStatus

```haskell
data TransactionStatus
  = Created
  | InProgress
  | Completed
  | Voided
  | Refunded
  deriving (Show, Eq, Ord, Generic, Read)
```

#### TransactionType

```haskell
data TransactionType
  = Sale
  | Return
  | Exchange
  | InventoryAdjustment
  | ManagerComp
  | Administrative
  deriving (Show, Eq, Ord, Generic, Read)
```

#### TransactionItem

```haskell
data TransactionItem = TransactionItem
  { transactionItemId :: UUID
  , transactionItemTransactionId :: UUID
  , transactionItemMenuItemSku :: UUID
  , transactionItemQuantity :: Int
  , transactionItemPricePerUnit :: Int
  , transactionItemDiscounts :: [DiscountRecord]
  , transactionItemTaxes :: [TaxRecord]
  , transactionItemSubtotal :: Int
  , transactionItemTotal :: Int
  }
```

#### PaymentTransaction

```haskell
data PaymentTransaction = PaymentTransaction
  { paymentId :: UUID
  , paymentTransactionId :: UUID
  , paymentMethod :: PaymentMethod
  , paymentAmount :: Int
  , paymentTendered :: Int
  , paymentChange :: Int
  , paymentReference :: Maybe Text
  , paymentApproved :: Bool
  , paymentAuthorizationCode :: Maybe Text
  }
```

## Database Schema

### menu_items Table

```sql
CREATE TABLE IF NOT EXISTS menu_items (
    sort INT NOT NULL,
    sku UUID PRIMARY KEY,
    brand TEXT NOT NULL,
    name TEXT NOT NULL,
    price INTEGER NOT NULL,
    measure_unit TEXT NOT NULL,
    per_package TEXT NOT NULL,
    quantity INT NOT NULL,
    category TEXT NOT NULL,
    subcategory TEXT NOT NULL,
    description TEXT NOT NULL,
    tags TEXT[] NOT NULL,
    effects TEXT[] NOT NULL
)
```

### strain_lineage Table

```sql
CREATE TABLE IF NOT EXISTS strain_lineage (
    sku UUID PRIMARY KEY REFERENCES menu_items(sku),
    thc TEXT NOT NULL,
    cbg TEXT NOT NULL,
    strain TEXT NOT NULL,
    creator TEXT NOT NULL,
    species TEXT NOT NULL,
    dominant_terpene TEXT NOT NULL,
    terpenes TEXT[] NOT NULL,
    lineage TEXT[] NOT NULL,
    leafly_url TEXT NOT NULL,
    img TEXT NOT NULL
)
```

### transaction Table

```sql
CREATE TABLE IF NOT EXISTS transaction (
  id UUID PRIMARY KEY,
  status TEXT NOT NULL,
  created TIMESTAMP WITH TIME ZONE NOT NULL,
  completed TIMESTAMP WITH TIME ZONE,
  customer_id UUID,
  employee_id UUID NOT NULL,
  register_id UUID NOT NULL,
  location_id UUID NOT NULL,
  subtotal INTEGER NOT NULL,
  discount_total INTEGER NOT NULL,
  tax_total INTEGER NOT NULL,
  total INTEGER NOT NULL,
  transaction_type TEXT NOT NULL,
  is_voided BOOLEAN NOT NULL DEFAULT FALSE,
  void_reason TEXT,
  is_refunded BOOLEAN NOT NULL DEFAULT FALSE,
  refund_reason TEXT,
  reference_transaction_id UUID,
  notes TEXT
)
```

Additional transaction-related tables:
- `transaction_item`: Stores line items within transactions
- `discount`: Stores applied discounts
- `transaction_tax`: Stores tax applications
- `payment_transaction`: Stores payment details
- `register`: Stores cash register information

## Transaction Processing

### Transaction Flow

1. **Creation**: Transaction is created with status `Created`
2. **Item Addition**: Items are added to the transaction
3. **Discount Application**: Optional discounts are applied
4. **Payment Addition**: One or more payments are added
5. **Finalization**: Transaction is finalized, changing status to `Completed`

### Transaction Operations

The system includes comprehensive transaction processing capabilities:

```haskell
createTransaction :: ConnectionPool -> Transaction -> IO Transaction
getTransaction :: ConnectionPool -> UUID -> IO (Maybe Transaction)
updateTransaction :: ConnectionPool -> UUID -> Transaction -> IO Transaction
addTransactionItem :: ConnectionPool -> TransactionItem -> IO TransactionItem
deleteTransactionItem :: ConnectionPool -> UUID -> IO ()
addPaymentTransaction :: ConnectionPool -> PaymentTransaction -> IO PaymentTransaction
deletePaymentTransaction :: ConnectionPool -> UUID -> IO ()
finalizeTransaction :: ConnectionPool -> UUID -> IO Transaction
```

### Refund and Void Operations

The system supports two key transaction reversal operations:

```haskell
voidTransaction :: ConnectionPool -> UUID -> Text -> IO Transaction
voidTransaction pool transactionId reason = do
  -- Update transaction status to VOIDED
  -- Mark transaction as voided with reason
  -- Return updated transaction
```

```haskell
refundTransaction :: ConnectionPool -> UUID -> Text -> IO Transaction
refundTransaction pool transactionId reason = do
  -- Get original transaction
  -- Create new inverse transaction with negative amounts
  -- Mark as Return type and reference original transaction
  -- Mark original as refunded
  -- Return new refund transaction
```

### Register Operations

The system supports cash register operations:

```haskell
openRegister :: ConnectionPool -> UUID -> OpenRegisterRequest -> IO Register
closeRegister :: ConnectionPool -> UUID -> CloseRegisterRequest -> IO CloseRegisterResult
```

## Security and Configuration

### CORS Configuration

The backend implements a permissive CORS policy suitable for development:

```haskell
corsPolicy =
  CorsResourcePolicy
    { corsOrigins = Nothing  -- Allow any origin
    , corsMethods = [methodGet, methodPost, methodPut, methodDelete, methodOptions]
    , corsRequestHeaders = [hContentType, hAccept, hAuthorization, hOrigin, hContentLength]
    , corsExposedHeaders = Nothing
    , corsMaxAge = Just 3600
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }
```

### Server Configuration

Server defaults:
- **Port**: 8080
- **Database**: localhost:5432/stockpile
- **Username**: Current system user (obtained via `getLoginName`)
- **Password**: "postgres"
- **Connection Pool Size**: 10

## Development Guidelines

### Environment Setup

1. Ensure PostgreSQL is installed and running
2. Use the provided connection details or update for your environment
3. Tables will be automatically created if they don't exist

### Build and Run

Build the project:
```bash
cabal build
```

Run the server:
```bash
cabal run
```

### Coding Patterns

The codebase employs several Haskell patterns:

1. **Resource Management**: Uses `withConnection` pattern to ensure resource cleanup
2. **Error Handling**: Structured error responses and exception handling
3. **Type Safety**: Leverages Haskell's type system for API definitions
4. **Functional Composition**: Pipeline-style data transformations

### Error Handling

The backend implements robust error handling:
- Try/catch blocks around database operations
- Descriptive error messages for client feedback
- Logging of errors to stderr for server monitoring
- Status message formatting for client consumption

### Database Optimization

The backend uses several database optimization techniques:
1. Connection pooling for efficient resource utilization
2. Prepared statements for safe query execution
3. Connection retry logic with exponential backoff
4. Resource cleanup via withConnection pattern