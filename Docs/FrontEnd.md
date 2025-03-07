# Cheeblr Frontend Documentation

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
- [Development Guidelines](#development-guidelines)

## Overview

The Cheeblr frontend is a single-page application (SPA) built with PureScript. It provides a comprehensive user interface for dispensary operations, including inventory management, point-of-sale functionality, and transaction processing. The application connects to a Haskell backend API to perform these operations.

## Technologies

The frontend utilizes several PureScript libraries and technologies:

- **Deku**: A declarative UI library for PureScript with hooks-like functionality
- **FRP**: Functional Reactive Programming principles for state management via the Poll mechanism
- **Routing**: Client-side routing using hash-based navigation
- **Fetch**: HTTP requests to the backend API
- **Yoga.JSON**: Data serialization and deserialization
- **Validation**: Form validation with semigroup-based validation
- **Finance.Money**: Precise handling of monetary values with the Discrete USD type

## Architecture

The application follows a component-based architecture pattern:

1. **Main Module**: The entry point that sets up routing and global state
2. **Components**: UI components for different views (LiveView, CreateTransaction, TransactionHistory, etc.)
3. **API Modules**: Separate modules for Inventory API and Transaction API
4. **Types**: Shared domain models and type definitions
5. **Services**: Business logic services like CashRegister, Ledger, and Compliance
6. **Validation**: Form validation logic and rules
7. **Utilities**: Helper functions and utilities

## Components

### Inventory Management

#### MenuLiveView
Displays the inventory items in a grid layout with filtering and sorting capabilities.

#### CreateItem / EditItem
Form components for adding and modifying inventory items with comprehensive validation.

#### DeleteItem
Confirmation component for removing inventory items.

### Transaction Processing

#### CreateTransaction
A comprehensive POS interface that allows:
- Adding items from inventory to the transaction
- Applying discounts and calculating taxes
- Processing various payment methods
- Finalizing sales transactions

```purescript
createTransaction = Deku.do
  -- State for transaction items, payments, etc.
  setItems /\ itemsValue <- useState []
  setPayments /\ paymentsValue <- useState []
  setInventory /\ inventoryValue <- useState []
  -- Form handling for transaction elements
  -- ...
```

#### TransactionHistory
Displays the history of transactions with filtering and detailed view capabilities.

```purescript
transactionHistory = Deku.do
  setTransactions /\ transactionsValue <- useState []
  setSelectedTransaction /\ selectedTransactionValue <- useState Nothing
  -- Load transactions and handle selection
  -- ...
```

### Cash Register Operations

Components for managing cash drawer operations:
- Opening/closing registers
- Cash drawer reconciliation
- Shift management

## Routing

The application uses hash-based routing with routes defined in the `Route` module:

```purescript
route :: RouteDuplex' Route
route = root $ G.sum
  { "LiveView": G.noArgs
  , "Create": "create" / G.noArgs
  , "Edit": "edit" / (string segment)
  , "Delete": "delete" / (string segment)
  }
```

Available routes include:
- `/` - Main inventory view (LiveView)
- `/create` - Create new item form
- `/edit/:uuid` - Edit an existing item by UUID
- `/delete/:uuid` - Delete confirmation for an item by UUID
- Additional routes for transaction management and POS operations

## State Management

The application uses Functional Reactive Programming (FRP) principles for state management:

- **Global State**: Managed through Poll mechanisms in the Main module
- **Component State**: Local state using Deku's hooks system (`useState`)
- **Shared State**: Key state like inventory is shared across components
- **Transaction State**: Complex state management for transaction processing

```purescript
-- Example of state hooks in component
setItems /\ itemsValue <- useState []
setPayments /\ paymentsValue <- useState []
setCustomerId /\ customerIdValue <- useState Nothing
```

## API Integration

The application includes separate API modules for different domains:

### Inventory API (`API.Inventory`)
- `readInventory`: Fetches all inventory items
- `writeInventory`: Creates a new inventory item
- `updateInventory`: Updates an existing inventory item
- `deleteInventory`: Deletes an inventory item by ID

### Transaction API (`API.Transaction`)
- `createTransaction`: Creates a new transaction
- `getTransaction`: Retrieves transaction details
- `getAllTransactions`: Gets transaction history
- `finalizeTransaction`: Completes a transaction
- `voidTransaction`: Voids a transaction
- `refundTransaction`: Processes a refund

## Data Models

### Inventory Models
- **MenuItem**: Complete product with all properties
- **StrainLineage**: Cannabis strain information
- **ItemCategory**: Product category enumeration
- **Species**: Cannabis species classification

### Transaction Models
- **Transaction**: Core transaction data
- **TransactionItem**: Line items within a transaction
- **PaymentTransaction**: Payment details
- **TransactionStatus**: Status enumeration (Created, InProgress, Completed, etc.)
- **PaymentMethod**: Payment type enumeration (Cash, Credit, Debit, etc.)

### Financial Models
- **DiscreteUSD**: Type-safe representation of monetary values
- **TaxRecord**: Tax application details
- **DiscountRecord**: Discount application details

## Form Handling

Forms are implemented using a combination of:

1. **Field Components**: Reusable form field components defined in the `Form` module
2. **State Hooks**: Each form field has associated state and validation state
3. **Live Validation**: Immediate feedback as users type
4. **Field Configuration**: Consistent configuration objects for field appearance and behavior

```purescript
makeTextField :: FieldConfig
              -> (String -> Effect Unit)
              -> (Maybe Boolean -> Effect Unit)
              -> Poll (Maybe Boolean)
              -> Boolean
              -> Nut
```

Form components handle:
- Text input with validation
- Dropdown selection
- Numeric input
- Multi-line text areas
- Payment processing forms

## Validation

The application includes comprehensive validation:

### Validation Rules
- Basic validations: nonEmpty, alphanumeric, maxLength
- Domain-specific: percentage, dollarAmount, validMeasurementUnit
- Complex validations: validUrl, nonNegativeInteger, commaList

### Validation Architecture
- Rule composition via `allOf` and `anyOf` combinators
- Semigroup-based collection of validation errors
- Real-time validation feedback
- Custom validation for domain types (UUID, ItemCategory, Species)

## Transaction Processing

The frontend implements the full transaction lifecycle:

### POS Workflow
1. Create a new transaction
2. Add items from inventory
3. Apply taxes and discounts
4. Process payments (multiple payment methods supported)
5. Finalize transaction

### Cash Register Operations
The CashRegister service handles:
- Opening/closing cash drawers
- Cash reconciliation
- Handling various payment types

### Transaction Modifications
Support for:
- Void operations
- Refund processing
- Transaction adjustments

## Services

### CashRegister Service
Handles cash drawer operations and transaction processing:

```purescript
data RegisterError
  = InvalidTransaction
  | PaymentRequired
  | InvalidPaymentAmount
  | InsufficientPayment
  | ProductNotFound
  | InventoryUnavailable
  | RegisterClosed
  | PermissionDenied
  | ReceiptPrinterError
  | NetworkError
  | InternalError String
```

### Ledger Service
Handles financial accounting operations:

```purescript
createSaleEntries ::
  Transaction ->
  UUID ->
  UUID ->
  UUID ->
  Aff (Either LedgerError (Array LedgerEntry))
```

### Compliance Service
Manages regulatory compliance requirements:

```purescript
checkCustomerEligibility ::
  UUID ->
  Maybe String ->
  Maybe Boolean ->
  UUID ->
  Aff (Either ComplianceError (Array CustomerVerification))
```

## Development Guidelines

### Project Setup
1. Clone the repository
2. Install dependencies using Spago: `spago install`
3. Start the development server: `npm run dev`

### Component Development
When creating new components:
1. Define the component in its own module
2. Use Deku hooks for state management
3. Implement proper validation for forms
4. Handle loading and error states

### Debugging
The application includes:
- Console logging for tracking component lifecycle
- Debug information panels in UI components
- Form validation state visualization

### Error Handling
Comprehensive error handling with:
- API error interception
- Form validation errors
- Network and system error handling
- User-friendly error messages

### Testing
The codebase includes warnings about:
- Unused names
- Unused type variables
- Shadowed names
- Type unification issues