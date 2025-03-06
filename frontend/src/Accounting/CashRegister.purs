module Accounting.CashRegister where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Array (foldl, null, filter, (:))
import Data.Finance.Currency (USD)
import Data.Finance.Money (Discrete(..), formatDiscrete)
import Data.Finance.Money.Format (numeric, numericC)
import Data.DateTime (DateTime)
import Data.UUID (UUID, genUUID, toString)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now)
import Types.Transaction
  ( MenuItem
  , Transaction
  , TransactionItem
  , TransactionStatus(..)
  , PaymentMethod(..)
  , DiscountType(..)
  , PaymentTransaction
  , TaxCategory(..)
  , TaxRecord
  , DiscountRecord
  , ItemCategory(..)
  , TransactionType(..)
  , ID
  )
import Ledger.System (LedgerError(..))

-- | Register operation errors
data RegisterError
  = InvalidTransaction
  | PaymentRequired
  | InvalidPaymentAmount
  | InsufficientPayment
  | ProductNotFound
  | InventoryUnavailable
  | RegisterClosed
  | LedgerError LedgerError
  | PermissionDenied
  | ReceiptPrinterError
  | NetworkError
  | InternalError String

derive instance eqRegisterError :: Eq RegisterError
derive instance ordRegisterError :: Ord RegisterError

instance showRegisterError :: Show RegisterError where
  show InvalidTransaction = "Invalid transaction"
  show PaymentRequired = "Payment required to complete transaction"
  show InvalidPaymentAmount = "Invalid payment amount"
  show InsufficientPayment = "Insufficient payment amount"
  show ProductNotFound = "Product not found"
  show InventoryUnavailable = "Product is not available in inventory"
  show RegisterClosed = "Register is closed"
  show (LedgerError e) = "Ledger error: " <> show e
  show PermissionDenied = "Permission denied for operation"
  show ReceiptPrinterError = "Receipt printer error"
  show NetworkError = "Network connection error"
  show (InternalError msg) = "Internal error: " <> msg

-- | Cash Register State
type RegisterState =
  { isOpen :: Boolean
  , currentDrawerAmount :: Discrete USD
  , currentTransaction :: Maybe Transaction
  , openedAt :: Maybe DateTime
  , openedBy :: Maybe (ID Employee)
  , lastTransactionTime :: Maybe DateTime
  , expectedDrawerAmount :: Discrete USD
  }

-- | Transaction Builder
type TransactionBuilder =
  { items :: Array TransactionItem
  , payments :: Array PaymentTransaction
  , customer :: Maybe (ID Customer)
  , employee :: ID Employee
  , register :: ID Register
  , location :: ID Location
  , discounts :: Array DiscountRecord
  , subtotal :: Discrete USD
  , taxTotal :: Discrete USD
  , total :: Discrete USD
  , status :: TransactionStatus
  , notes :: Maybe String
  }

-- | Initialize a new empty transaction
initializeTransaction :: 
  ID Employee -> 
  ID Register -> 
  ID Location -> 
  Aff TransactionBuilder
initializeTransaction employeeId registerId locationId = do
  liftEffect $ log "Initializing new transaction"
  
  pure {
    items: [],
    payments: [],
    customer: Nothing,
    employee: employeeId,
    register: registerId,
    location: locationId,
    discounts: [],
    subtotal: Discrete 0,
    taxTotal: Discrete 0,
    total: Discrete 0,
    status: Created,
    notes: Nothing
  }

-- | Add a menu item to the transaction
addItemToTransaction :: 
  TransactionBuilder -> 
  MenuItem -> 
  Number -> 
  Aff (Either RegisterError TransactionBuilder)
addItemToTransaction builder menuItem quantity = do
  liftEffect $ log $ "Adding item to transaction: " <> menuItem.name
  
  -- Check if item is available
  if menuItem.quantity <= 0 
    then do
      liftEffect $ log "Item is out of stock"
      pure $ Left InventoryUnavailable
    else if quantity <= 0.0
      then do
        liftEffect $ log "Invalid quantity"
        pure $ Left InvalidTransaction
      else do
        -- Generate transaction item ID
        itemId <- liftEffect genUUID
        
        -- Calculate item price and tax
        let 
          itemPrice = Discrete (floor (menuItem.price * 100.0))
          itemSubtotal = itemPrice * (Discrete (floor quantity))
          
          -- Calculate taxes based on product type
          taxes = calculateTaxes itemSubtotal menuItem
          itemTaxTotal = foldl (\acc tax -> acc + tax.amount) (Discrete 0) taxes
          
          -- Create new transaction item
          newItem = 
            { id: itemId
            , transactionId: dummyTransactionId -- Will be set when transaction is finalized
            , menuItemSku: menuItem.sku
            , quantity
            , pricePerUnit: itemPrice
            , discounts: []
            , taxes
            , subtotal: itemSubtotal
            , total: itemSubtotal + itemTaxTotal
            }
          
          -- Update transaction totals
          newSubtotal = builder.subtotal + itemSubtotal
          newTaxTotal = builder.taxTotal + itemTaxTotal
          newTotal = newSubtotal + newTaxTotal
          
          -- Updated builder
          updatedBuilder = builder { 
            items = newItem : builder.items,
            subtotal = newSubtotal,
            taxTotal = newTaxTotal,
            total = newTotal,
            status = InProgress
          }
        
        liftEffect $ log $ "Item added: " <> menuItem.name <> 
                         ", Quantity: " <> show quantity <> 
                         ", Price: " <> formatDiscrete numericC itemPrice
        
        pure $ Right updatedBuilder

-- | Apply a discount to the transaction
applyDiscount :: 
  TransactionBuilder -> 
  DiscountType -> 
  String -> 
  Maybe (ID Employee) -> 
  Aff (Either RegisterError TransactionBuilder)
applyDiscount builder discountType reason maybeApprover = do
  liftEffect $ log "Applying discount to transaction"
  
  if builder.subtotal == Discrete 0
    then do
      liftEffect $ log "Cannot apply discount to empty transaction"
      pure $ Left InvalidTransaction
    else do
      -- Calculate discount amount
      let 
        discountAmount = case discountType of
          PercentOff percentage -> 
            let 
              discountValue = builder.subtotal * (Discrete (floor (percentage * 100.0))) / (Discrete 100)
            in
              discountValue
          
          AmountOff amount -> 
            if amount > builder.subtotal
              then builder.subtotal
              else amount
          
          BuyOneGetOne -> 
            -- This is simplified - real BOGO would be more complex
            builder.subtotal / (Discrete 2)
          
          Custom _ amount -> 
            if amount > builder.subtotal
              then builder.subtotal
              else amount
        
        -- Create discount record
        newDiscount = 
          { type: discountType
          , amount: discountAmount
          , reason
          , approvedBy: maybeApprover
          }
        
        -- Update transaction totals
        newTotal = builder.subtotal - discountAmount + builder.taxTotal
        
        -- Updated builder
        updatedBuilder = builder {
          discounts = newDiscount : builder.discounts,
          total = newTotal
        }
      
      liftEffect $ log $ "Discount applied: " <> formatDiscrete numericC discountAmount
      
      pure $ Right updatedBuilder

-- | Add a payment to the transaction
addPayment :: 
  TransactionBuilder -> 
  PaymentMethod -> 
  Discrete USD -> 
  Discrete USD -> 
  Maybe String -> 
  Aff (Either RegisterError TransactionBuilder)
addPayment builder method amount tendered reference = do
  liftEffect $ log $ "Adding payment: " <> show method <> " " <> formatDiscrete numericC amount
  
  if amount <= Discrete 0
    then do
      liftEffect $ log "Invalid payment amount"
      pure $ Left InvalidPaymentAmount
    else do
      -- Generate payment ID
      paymentId <- liftEffect genUUID
      
      -- Calculate remaining balance and change
      let
        currentPaymentTotal = foldl (\acc p -> acc + p.amount) (Discrete 0) builder.payments
        remainingBalance = builder.total - currentPaymentTotal
        
        -- Ensure payment doesn't exceed remaining balance
        actualPaymentAmount = if amount > remainingBalance 
                               then remainingBalance
                               else amount
        
        -- Calculate change (only applies to cash payments)
        change = if method == Cash && tendered > actualPaymentAmount
                  then tendered - actualPaymentAmount
                  else Discrete 0
        
        -- Create payment transaction
        newPayment = 
          { id: paymentId
          , transactionId: dummyTransactionId -- Will be set when transaction is finalized
          , method
          , amount: actualPaymentAmount
          , tendered: if method == Cash then tendered else actualPaymentAmount
          , change
          , reference
          , approved: true -- Would come from payment processor in real implementation
          , authorizationCode: Nothing -- Would come from payment processor
          }
        
        -- Update payments and check if transaction is fully paid
        newPayments = newPayment : builder.payments
        newPaymentTotal = currentPaymentTotal + actualPaymentAmount
        
        -- Update transaction status
        newStatus = if newPaymentTotal >= builder.total
                     then Completed
                     else InProgress
        
        -- Updated builder
        updatedBuilder = builder {
          payments = newPayments,
          status = newStatus
        }
      
      liftEffect $ log $ "Payment added: " <> show method <> 
                       ", Amount: " <> formatDiscrete numericC actualPaymentAmount <>
                       (if change > Discrete 0 
                         then ", Change: " <> formatDiscrete numericC change
                         else "")
      
      pure $ Right updatedBuilder

-- | Complete the transaction
finalizeTransaction :: 
  TransactionBuilder -> 
  Aff (Either RegisterError Transaction)
finalizeTransaction builder = do
  liftEffect $ log "Finalizing transaction"
  
  -- Check if transaction can be completed
  if null builder.items
    then do
      liftEffect $ log "Cannot finalize transaction with no items"
      pure $ Left InvalidTransaction
    else do
      -- Calculate the total payments received
      let totalPayments = foldl (\acc p -> acc + p.amount) (Discrete 0) builder.payments
      
      if totalPayments < builder.total
        then do
          liftEffect $ log "Insufficient payment to complete transaction"
          pure $ Left InsufficientPayment
        else do
          -- Generate transaction ID
          transactionId <- liftEffect genUUID
          
          -- Get current timestamp
          timestamp <- liftEffect now
          
          -- Calculate discount total
          let discountTotal = foldl (\acc d -> acc + d.amount) (Discrete 0) builder.discounts
          
          -- Update items and payments with transaction ID
          let 
            updatedItems = map (\item -> item { transactionId = transactionId }) builder.items
            updatedPayments = map (\payment -> payment { transactionId = transactionId }) builder.payments
            
            -- Create the final transaction
            transaction = 
              { id: transactionId
              , status: Completed
              , created: timestamp
              , completed: Just timestamp
              , customer: builder.customer
              , employee: builder.employee
              , register: builder.register
              , location: builder.location
              , items: updatedItems
              , payments: updatedPayments
              , subtotal: builder.subtotal
              , discountTotal
              , taxTotal: builder.taxTotal
              , total: builder.total
              , transactionType: Sale
              , isVoided: false
              , voidReason: Nothing
              , isRefunded: false
              , refundReason: Nothing
              , referenceTransactionId: Nothing
              , notes: builder.notes
              }
          
          liftEffect $ log $ "Transaction finalized: " <> toString transactionId <>
                           ", Total: " <> formatDiscrete numericC builder.total
          
          pure $ Right transaction

-- | Generate a receipt from a transaction
generateReceipt :: Transaction -> String
generateReceipt transaction =
  let
    receiptHeader = 
      "===================================\n" <>
      "        CANNABIS DISPENSARY        \n" <>
      "===================================\n" <>
      "Transaction: " <> toString transaction.id <> "\n" <>
      "Date: " <> show transaction.created <> "\n" <>
      "-----------------------------------\n\n"
    
    itemLines = foldl (\acc item -> acc <> formatTransactionItem item) "" transaction.items
    
    subtotalLine = 
      "-----------------------------------\n" <>
      "Subtotal:         " <> formatDiscrete numeric transaction.subtotal <> "\n"
    
    discountLine = 
      if transaction.discountTotal > Discrete 0
        then "Discount:         -" <> formatDiscrete numeric transaction.discountTotal <> "\n"
        else ""
    
    taxLine = "Tax:              " <> formatDiscrete numeric transaction.taxTotal <> "\n"
    
    totalLine = 
      "TOTAL:            " <> formatDiscrete numericC transaction.total <> "\n\n"
    
    paymentLines = foldl (\acc payment -> acc <> formatPayment payment) "" transaction.payments
    
    receiptFooter =
      "===================================\n" <>
      "       THANK YOU FOR VISITING      \n" <>
      "===================================\n"
  in
    receiptHeader <> itemLines <> subtotalLine <> discountLine <> taxLine <> totalLine <> paymentLines <> receiptFooter

-- | Format a transaction item for receipt
formatTransactionItem :: TransactionItem -> String
formatTransactionItem item =
  let
    itemLine = 
      (if item.quantity /= 1.0 
       then show item.quantity <> " x "
       else "") <>
      "Item @ " <> formatDiscrete numeric item.pricePerUnit <> "\n"
    
    taxLines = foldl (\acc tax -> 
                "  " <> tax.description <> " (" <> show (tax.rate * 100.0) <> "%): " <> 
                formatDiscrete numeric tax.amount <> "\n") 
              "" 
              item.taxes
    
    totalLine = "  Item Total: " <> formatDiscrete numeric item.total <> "\n\n"
  in
    itemLine <> taxLines <> totalLine

-- | Format a payment for receipt
formatPayment :: PaymentTransaction -> String
formatPayment payment =
  let
    paymentLine = "Paid (" <> show payment.method <> "): " <> formatDiscrete numeric payment.amount <> "\n"
    
    changeLine = 
      if payment.change > Discrete 0
        then "Change: " <> formatDiscrete numeric payment.change <> "\n"
        else ""
  in
    paymentLine <> changeLine

-- | Open a register
openRegister :: 
  ID Register -> 
  ID Employee -> 
  Discrete USD -> 
  Aff (Either RegisterError RegisterState)
openRegister registerId employeeId startingCash = do
  timestamp <- liftEffect now
  
  liftEffect $ log $ "Opening register " <> toString registerId <> 
                   " with " <> formatDiscrete numericC startingCash
  
  -- In a real implementation, this would check permissions and record the opening in a database
  
  pure $ Right {
    isOpen: true,
    currentDrawerAmount: startingCash,
    currentTransaction: Nothing,
    openedAt: Just timestamp,
    openedBy: Just employeeId,
    lastTransactionTime: Nothing,
    expectedDrawerAmount: startingCash
  }

-- | Close a register
closeRegister :: 
  RegisterState -> 
  ID Employee -> 
  Discrete USD -> 
  Aff (Either RegisterError { closingState :: RegisterState, variance :: Discrete USD })
closeRegister state employeeId countedCash = do
  timestamp <- liftEffect now
  
  liftEffect $ log $ "Closing register with counted amount: " <> formatDiscrete numericC countedCash
  
  if not state.isOpen
    then do
      liftEffect $ log "Cannot close register that is not open"
      pure $ Left RegisterClosed
    else do
      -- Calculate variance
      let variance = countedCash - state.expectedDrawerAmount
      
      -- Updated register state
      let closedState = state {
        isOpen = false,
        currentDrawerAmount = countedCash,
        currentTransaction = Nothing,
        lastTransactionTime = Just timestamp
      }
      
      liftEffect $ log $ "Register closed with variance: " <> formatDiscrete numericC variance
      
      pure $ Right {
        closingState: closedState,
        variance
      }

-- | Calculate taxes for an item based on product category
calculateTaxes :: Discrete USD -> MenuItem -> Array TaxRecord
calculateTaxes amount menuItem =
  let
    -- Example tax rates - these would come from configuration or database in real implementation
    salesTaxRate = 0.08 -- 8%
    cannabisTaxRate = 0.15 -- 15% for cannabis products
    
    -- Determine if this is a cannabis product with cannabis tax
    isCannabisProduct = case menuItem.category of
      Flower -> true
      PreRolls -> true
      Vaporizers -> true
      Edibles -> true
      Drinks -> true
      Concentrates -> true
      Topicals -> true
      Tinctures -> true
      _ -> false
    
    -- Calculate tax amounts
    salesTaxAmount = amount * (Discrete (floor (salesTaxRate * 100.0))) / (Discrete 100)
    cannabisTaxAmount = 
      if isCannabisProduct
        then amount * (Discrete (floor (cannabisTaxRate * 100.0))) / (Discrete 100)
        else Discrete 0
    
    -- Create tax records
    salesTax = 
      { category: RegularSalesTax
      , rate: salesTaxRate
      , amount: salesTaxAmount
      , description: "Sales Tax"
      }
    
    cannabisTax = 
      { category: CannabisTax
      , rate: cannabisTaxRate
      , amount: cannabisTaxAmount
      , description: "Cannabis Excise Tax"
      }
  in
    if cannabisTaxAmount > Discrete 0
      then [salesTax, cannabisTax]
      else [salesTax]

-- | Process a refund
processRefund :: 
  Transaction -> 
  Array (ID TransactionItem) -> 
  String -> 
  ID Employee -> 
  Aff (Either RegisterError Transaction)
processRefund originalTransaction itemIdsToRefund reason employeeId = do
  liftEffect $ log $ "Processing refund for transaction " <> toString originalTransaction.id
  
  -- Check if transaction can be refunded
  if originalTransaction.isRefunded
    then do
      liftEffect $ log "Transaction has already been refunded"
      pure $ Left $ InternalError "Transaction has already been refunded"
    else if originalTransaction.isVoided
      then do
        liftEffect $ log "Cannot refund a voided transaction"
        pure $ Left $ InternalError "Cannot refund a voided transaction"
      else do
        -- Generate new transaction ID and timestamp
        refundId <- liftEffect genUUID
        timestamp <- liftEffect now
        
        -- Determine which items to refund
        let 
          itemsToRefund = 
            if null itemIdsToRefund
              then originalTransaction.items -- Full refund
              else filter (\item -> contains itemIdsToRefund item.id) originalTransaction.items
          
          -- Calculate refund amount
          refundSubtotal = foldl (\acc item -> acc + item.subtotal) (Discrete 0) itemsToRefund
          refundTaxTotal = foldl (\acc item -> 
                              foldl (\acc2 tax -> acc2 + tax.amount) acc item.taxes
                            ) (Discrete 0) itemsToRefund
          refundTotal = refundSubtotal + refundTaxTotal
          
          -- Create refund transaction (simplified - real implementation would be more complex)
          -- Create a refund payment
          refundPayment = 
            { id: dummyPaymentId
            , transactionId: refundId
            , method: Cash -- In real implementation would match original payment method
            , amount: negate refundTotal
            , tendered: negate refundTotal
            , change: Discrete 0
            , reference: Nothing
            , approved: true
            , authorizationCode: Nothing
            }
          
          -- Create the refund transaction
          refundTransaction = 
            { id: refundId
            , status: Completed
            , created: timestamp
            , completed: Just timestamp
            , customer: originalTransaction.customer
            , employee: employeeId
            , register: originalTransaction.register
            , location: originalTransaction.location
            , items: map makeRefundItem itemsToRefund
            , payments: [refundPayment]
            , subtotal: negate refundSubtotal
            , discountTotal: Discrete 0
            , taxTotal: negate refundTaxTotal
            , total: negate refundTotal
            , transactionType: Return
            , isVoided: false
            , voidReason: Nothing
            , isRefunded: false
            , refundReason: Just reason
            , referenceTransactionId: Just originalTransaction.id
            , notes: Just $ "Refund for transaction " <> toString originalTransaction.id
            }
        
        liftEffect $ log $ "Refund processed: " <> toString refundId <> 
                         ", Amount: " <> formatDiscrete numericC refundTotal
        
        pure $ Right refundTransaction
  where
    -- Check if array contains an ID
    contains :: forall a. Array (ID a) -> ID a -> Boolean
    contains ids id = 
      case ids of
        [] -> false
        (x:xs) -> if x == id then true else contains xs id
    
    -- Convert transaction item to a refund item
    makeRefundItem :: TransactionItem -> TransactionItem
    makeRefundItem item = 
      item { 
        transactionId = dummyTransactionId, -- Will be updated later
        subtotal = negate item.subtotal,
        total = negate item.total,
        taxes = map (\tax -> tax { amount = negate tax.amount }) item.taxes
      }

-- Helper functions for array operations
null :: forall a. Array a -> Boolean
null [] = true
null _ = false

-- Helper function for array length
length :: forall a. Array a -> Int
length xs = go 0 xs
  where
    go :: Int -> Array a -> Int
    go acc [] = acc
    go acc (_:rest) = go (acc + 1) rest

-- Dummy values for example purposes only
dummyTransactionId :: UUID
dummyTransactionId = unsafeCoerce "00000000-0000-0000-0000-000000000001"

dummyPaymentId :: UUID
dummyPaymentId = unsafeCoerce "00000000-0000-0000-0000-000000000002"

-- | Unsafe coerce for example purposes only
foreign import unsafeCoerce :: forall a b. a -> b