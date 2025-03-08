module Services.CashRegister where

import Prelude
import Types.Inventory
import Types.Transaction
import Types.Transaction
import Utils.Money

import Control.Extend (duplicate)
import Data.Array (foldl, null, filter, (:))
import Data.Array as Array
import Data.Bounded (bottom)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant, toDateTime)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..))
import Data.Finance.Currency (USD)
import Data.Finance.Money (Discrete(..), formatDiscrete)
import Data.Finance.Money.Extended (DiscreteMoney, fromDiscrete', toDiscrete)
import Data.Finance.Money.Format (numeric, numericC)
import Data.Int as Int
import Data.JSDate (fromInstant)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe (Maybe)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Deku.DOM.SVG.Attributes (d)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now, nowDateTime)
import Types.UUID (UUID(..))
import Utils.Formatting (uuidToString)
import Utils.UUIDGen (genUUID)
import Web.Event.Event (timeStamp)

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
  show PermissionDenied = "Permission denied for operation"
  show ReceiptPrinterError = "Receipt printer error"
  show NetworkError = "Network connection error"
  show (InternalError msg) = "Internal error: " <> msg

type RegisterState =
  { isOpen :: Boolean
  , currentDrawerAmount :: Discrete USD
  , currentTransaction :: Maybe Transaction
  , openedAt :: Maybe DateTime
  , openedBy :: Maybe UUID
  , lastTransactionTime :: Maybe DateTime
  , expectedDrawerAmount :: Discrete USD
  }

type TransactionBuilder =
  { items :: Array TransactionItem
  , payments :: Array PaymentTransaction
  , customer :: Maybe UUID
  , employee :: UUID
  , register :: UUID
  , location :: UUID
  , discounts :: Array DiscountRecord
  , subtotal :: Discrete USD
  , taxTotal :: Discrete USD
  , total :: Discrete USD
  , status :: TransactionStatus
  , notes :: Maybe String
  }

initializeTransaction
  :: UUID
  -> UUID
  -> UUID
  -> Aff TransactionBuilder
initializeTransaction employeeId registerId locationId = do
  liftEffect $ log "Initializing new transaction"

  pure
    { items: []
    , payments: []
    , customer: Nothing
    , employee: employeeId
    , register: registerId
    , location: locationId
    , discounts: []
    , subtotal: Discrete 0
    , taxTotal: Discrete 0
    , total: Discrete 0
    , status: Created
    , notes: Nothing
    }

addItemToTransaction
  :: TransactionBuilder
  -> MenuItem
  -> Number
  -> Aff (Either RegisterError TransactionBuilder)
addItemToTransaction builder menuItem quantity = do
  liftEffect $ log $ "Adding item to transaction: " <> menuItem.name

  if menuItem.quantity <= 0 then do
    liftEffect $ log "Item is out of stock"
    pure $ Left InventoryUnavailable
  else if quantity <= 0.0 then do
    liftEffect $ log "Invalid quantity"
    pure $ Left InvalidTransaction
  else do

    itemId <- liftEffect genUUID

    let
      itemPrice = Discrete (menuItem.price * 100.0)
      itemSubtotal = itemPrice * (Discrete (quantity))

      taxes = calculateTaxes itemSubtotal menuItem
      itemTaxTotal = foldl (\acc tax -> acc + tax.amount) (Discrete 0) taxes

      newItem =
        { id: itemId
        , transactionId: dummyTransactionId
        , menuItemSku: menuItem.sku
        , quantity
        , pricePerUnit: fromDiscrete' itemPrice
        , discounts: []
        , taxes
        , subtotal: fromDiscrete' itemSubtotal
        , total: fromDiscrete' (itemSubtotal + itemTaxTotal)
        }

      newSubtotal = builder.subtotal + itemSubtotal
      newTaxTotal = builder.taxTotal + itemTaxTotal
      newTotal = newSubtotal + newTaxTotal

      updatedBuilder = builder
        { items = newItem : builder.items
        , subtotal = newSubtotal
        , taxTotal = newTaxTotal
        , total = newTotal
        , status = InProgress
        }

    liftEffect $ log $ "Item added: " <> menuItem.name
      <> ", Quantity: "
      <> show quantity
      <> ", Price: "
      <> formatDiscrete numericC itemPrice

    pure $ Right updatedBuilder

applyDiscount
  :: TransactionBuilder
  -> DiscountType
  -> String
  -> Maybe UUID
  -> Aff (Either RegisterError TransactionBuilder)
applyDiscount builder discountType reason maybeApprover = do
  liftEffect $ log "Applying discount to transaction"

  if builder.subtotal == Discrete 0 then do
    liftEffect $ log "Cannot apply discount to empty transaction"
    pure $ Left InvalidTransaction
  else do

    let
      discountAmount = case discountType of
        PercentOff percentage ->
          let
            discountValue = builder.subtotal * (Discrete (percentage * 100.0)) /
              (Discrete 100)
          in
            discountValue

        AmountOff amount ->
          if amount > builder.subtotal then builder.subtotal
          else amount

        BuyOneGetOne ->

          builder.subtotal / (Discrete 2)

        Custom _ amount ->
          if amount > builder.subtotal then builder.subtotal
          else amount

      newDiscount =
        { type: discountType
        , amount: fromDiscrete' discountAmount
        , reason
        , approvedBy: maybeApprover
        }

      newTotal = builder.subtotal - discountAmount + builder.taxTotal

      updatedBuilder = builder
        { discounts = newDiscount : builder.discounts
        , total = newTotal
        }

    liftEffect $ log $ "Discount applied: " <> formatDiscrete numericC
      discountAmount

    pure $ Right updatedBuilder

addPayment
  :: TransactionBuilder
  -> PaymentMethod
  -> Discrete USD
  -> Discrete USD
  -> Maybe String
  -> Aff (Either RegisterError TransactionBuilder)
addPayment builder method amount tendered reference = do
  liftEffect $ log $ "Adding payment: " <> show method <> " " <> formatDiscrete
    numericC
    amount

  if amount <= Discrete 0 then do
    liftEffect $ log "Invalid payment amount"
    pure $ Left InvalidPaymentAmount
  else do

    paymentId <- liftEffect genUUID

    let
      currentPaymentTotal = foldl (\acc p -> acc + (toDiscrete p.amount)) (Discrete 0)
        builder.payments
      remainingBalance = builder.total - currentPaymentTotal

      actualPaymentAmount =
        if amount > remainingBalance then remainingBalance
        else amount

      change =
        if method == Cash && tendered > actualPaymentAmount then tendered -
          actualPaymentAmount
        else Discrete 0

      newPayment =
        { id: paymentId
        , transactionId: dummyTransactionId
        , method
        , amount: fromDiscrete' actualPaymentAmount
        , tendered: fromDiscrete' (if method == Cash then tendered else actualPaymentAmount)
        , change: fromDiscrete' change
        , reference
        , approved: true
        , authorizationCode: Nothing
        }

      newPayments = newPayment : builder.payments
      newPaymentTotal = currentPaymentTotal + actualPaymentAmount

      newStatus =
        if newPaymentTotal >= builder.total then Completed
        else InProgress

      updatedBuilder = builder
        { payments = newPayments
        , status = newStatus
        }

    liftEffect $ log $ "Payment added: " <> show method
      <> ", Amount: "
      <> formatDiscrete numericC actualPaymentAmount
      <>
        ( if change > Discrete 0 then ", Change: " <> formatDiscrete numericC
            change
          else ""
        )

    pure $ Right updatedBuilder

finalizeTransaction
  :: TransactionBuilder
  -> Aff (Either RegisterError Transaction)
finalizeTransaction builder = do
  liftEffect $ log "Finalizing transaction"

  if null builder.items then do
    liftEffect $ log "Cannot finalize transaction with no items"
    pure $ Left InvalidTransaction
  else do
    -- Get the total payments using PaymentTransaction directly
    let
      totalPayments = foldl
        ( \acc payment ->
            case payment of
              PaymentTransaction p -> acc + (toDiscrete p.amount)
        )
        (Discrete 0)
        builder.payments

    if totalPayments < builder.total then do
      liftEffect $ log "Insufficient payment to complete transaction"
      pure $ Left InsufficientPayment
    else do
      transactionId <- liftEffect genUUID
      timestamp <- liftEffect nowDateTime

      let
        discountTotal = foldl (\acc d -> acc + (toDiscrete d.amount))
          (Discrete 0)
          builder.discounts

      let
        updatedItems = map
          ( \item ->
              let
                TransactionItem ti = item
              in
                TransactionItem (ti { transactionId = transactionId })
          )
          builder.items

        updatedPayments = map
          ( \payment ->
              let
                PaymentTransaction p = payment
              in
                PaymentTransaction (p { transactionId = transactionId })
          )
          builder.payments

        transaction = Transaction
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
          , subtotal: fromDiscrete' builder.subtotal
          , discountTotal: fromDiscrete' discountTotal
          , taxTotal: fromDiscrete' builder.taxTotal
          , total: fromDiscrete' builder.total
          , transactionType: Sale
          , isVoided: false
          , voidReason: Nothing
          , isRefunded: false
          , refundReason: Nothing
          , referenceTransactionId: Nothing
          , notes: builder.notes
          }

      liftEffect $ log $ "Transaction finalized: " <> uuidToString transactionId
        <> ", Total: "
        <> formatDiscrete numericC builder.total

      pure $ Right transaction

generateReceipt :: Transaction -> String
generateReceipt transaction =
  let
    txData = unwrap transaction

    receiptHeader =
      "===================================\n"
        <> "        CANNABIS DISPENSARY        \n"
        <> "===================================\n"
        <> "Transaction: "
        <> uuidToString txData.id
        <> "\n"
        <> "Date: "
        <> show txData.created
        <> "\n"
        <>
          "\n"

    itemLines = foldl (\acc item -> acc <> formatTransactionItem item) ""
      txData.items

    subtotalLine =
      "\n"
        <> "Subtotal:         "
        <> formatDiscrete numeric (toDiscrete txData.subtotal)
        <> "\n"

    discountLine =
      if txData.discountTotal > (fromDiscrete' (Discrete 0)) then
        "Discount:         -"
          <> formatDiscrete numeric (toDiscrete txData.discountTotal)
          <> "\n"
      else ""

    taxLine = "Tax:              "
      <> formatDiscrete numeric (toDiscrete txData.taxTotal)
      <> "\n"

    totalLine =
      "TOTAL:            " <> formatDiscrete numericC (toDiscrete txData.total)
        <> "\n\n"

    paymentLines = foldl (\acc payment -> acc <> formatPayment payment) ""
      txData.payments

    receiptFooter =
      "===================================\n"
        <> "       THANK YOU FOR VISITING      \n"
        <>
          "===================================\n"
  in
    receiptHeader <> itemLines <> subtotalLine <> discountLine <> taxLine
      <> totalLine
      <> paymentLines
      <> receiptFooter

formatTransactionItem :: TransactionItem -> String
formatTransactionItem (TransactionItem item) =
  let
    itemLine =
      ( if item.quantity /= 1.0 then show item.quantity <> " x "
        else ""
      )
        <> "Item @ "
        <> formatDiscrete numeric (toDiscrete item.pricePerUnit)
        <> "\n"

    taxLines = foldl
      ( \acc tax ->
          acc <> "  " <> tax.description <> " (" <> show (tax.rate * 100.0) <> "%): "
            <> formatDiscrete numeric (toDiscrete tax.amount)
            <> "\n"
      )
      ""
      item.taxes

    totalLine = "  Item Total: "
      <> formatDiscrete numeric (toDiscrete item.total)
      <> "\n\n"
  in
    itemLine <> taxLines <> totalLine

formatPayment :: PaymentTransaction -> String
formatPayment (PaymentTransaction payment) =
  let
    paymentLine = "Paid (" <> show payment.method <> "): "
      <> formatDiscrete numeric (toDiscrete payment.amount)
      <> "\n"

    changeLine =
      if payment.change > (fromDiscrete' (Discrete 0)) then "Change: "
        <> formatDiscrete numeric (toDiscrete payment.change)
        <> "\n"
      else ""
  in
    paymentLine <> changeLine

openRegister
  :: UUID
  -> UUID
  -> Discrete USD
  -> Aff (Either RegisterError RegisterState)
openRegister registerId employeeId startingCash = do
  currentTime <- liftEffect now
  timestamp <- liftEffect nowDateTime

  liftEffect $ log $ "Opening register " <> uuidToString registerId
    <> " with "
    <> formatDiscrete numericC startingCash

  pure $ Right
    { isOpen: true
    , currentDrawerAmount: startingCash
    , currentTransaction: Nothing
    , openedAt: Just timestamp
    ,
      openedBy: Just employeeId
    , lastTransactionTime: Nothing
    , expectedDrawerAmount: startingCash
    }

closeRegister
  :: RegisterState
  -> UUID
  -> Discrete USD
  -> Aff
       ( Either RegisterError
           { closingState :: RegisterState, variance :: Discrete USD }
       )
closeRegister state employeeId countedCash = do
  timestamp <- liftEffect nowDateTime

  liftEffect $ log $ "Closing register with counted amount: " <> formatDiscrete
    numericC
    countedCash

  if not state.isOpen then do
    liftEffect $ log "Cannot close register that is not open"
    pure $ Left RegisterClosed
  else do

    let variance = countedCash - state.expectedDrawerAmount

    let
      closedState = state
        { isOpen = false
        , currentDrawerAmount = countedCash
        , currentTransaction = Nothing
        , lastTransactionTime = Just timestamp
        }

    liftEffect $ log $ "Register closed with variance: " <> formatDiscrete
      numericC
      variance

    pure $ Right
      { closingState: closedState
      , variance
      }

calculateTaxes :: Discrete USD -> MenuItem -> Array TaxRecord
calculateTaxes amount menuItem =
  let
    -- Unwrap the MenuItem to access its properties correctly
    MenuItem menuItemRecord = menuItem
    
    salesTaxRate = 0.08
    cannabisTaxRate = 0.15

    isCannabisProduct = case menuItemRecord.category of
      Flower -> true
      PreRolls -> true
      Vaporizers -> true
      Edibles -> true
      Drinks -> true
      Concentrates -> true
      Topicals -> true
      Tinctures -> true
      _ -> false

    -- Convert the Discrete USD to an Int value we can work with
    amountInCents = toDiscrete amount
    
    -- Calculate tax amounts using integer arithmetic
    salesTaxAmount = Discrete (Int.floor (Int.toNumber amountInCents * salesTaxRate))
    cannabisTaxAmount =
      if isCannabisProduct then 
        Discrete (Int.floor (Int.toNumber amountInCents * cannabisTaxRate))
      else Discrete 0

    salesTax =
      { category: RegularSalesTax
      , rate: salesTaxRate
      , amount: fromDiscrete' salesTaxAmount
      , description: "Sales Tax"
      }

    cannabisTax =
      { category: CannabisTax
      , rate: cannabisTaxRate
      , amount: fromDiscrete' cannabisTaxAmount
      , description: "Cannabis Excise Tax"
      }
  in
    if cannabisTaxAmount > Discrete 0 then [ salesTax, cannabisTax ]
    else [ salesTax ]

processRefund :: forall t. { id :: UUID | t } -> Array UUID -> String -> UUID -> Aff (Either RegisterError Transaction)
processRefund originalTransaction itemIdsToRefund reason employeeId = do
  liftEffect $ log $ "Processing refund for transaction " <> uuidToString
    originalTransaction.id

  -- Get current timestamp for use in the transaction
  timestamp <- liftEffect nowDateTime

  let txId = originalTransaction.id
  -- We need to create a minimally viable Transaction that has all required fields
  -- Creating a placeholder transaction with all required fields
  let txData = unwrap (Transaction 
        { id: txId
        , status: Completed
        , created: timestamp
        , completed: Just timestamp
        , customer: Nothing 
        , employee: employeeId
        , register: dummyTransactionId
        , location: dummyTransactionId
        , items: []
        , payments: []
        , subtotal: fromDiscrete' (Discrete 0)
        , discountTotal: fromDiscrete' (Discrete 0)
        , taxTotal: fromDiscrete' (Discrete 0)
        , total: fromDiscrete' (Discrete 0)
        , transactionType: Sale
        , isVoided: false
        , voidReason: Nothing
        , isRefunded: false
        , refundReason: Nothing
        , referenceTransactionId: Nothing
        , notes: Nothing
        })

  if txData.isRefunded then do
    liftEffect $ log "Transaction has already been refunded"
    pure $ Left $ InternalError "Transaction has already been refunded"
  else if txData.isVoided then do
    liftEffect $ log "Cannot refund a voided transaction"
    pure $ Left $ InternalError "Cannot refund a voided transaction"
  else do

    refundId <- liftEffect genUUID
    timestamp <- liftEffect nowDateTime

    let
      itemsToRefund =
        if null itemIdsToRefund then txData.items
        else filter (\item -> contains itemIdsToRefund (unwrap item).id)
          txData.items

      refundSubtotal = foldl (\acc item -> acc + (toDiscrete (unwrap item).subtotal)) (Discrete 0)
        itemsToRefund
      refundTaxTotal = foldl
        ( \acc item ->
            foldl (\acc2 tax -> acc2 + (toDiscrete tax.amount)) acc (unwrap item).taxes
        )
        (Discrete 0)
        itemsToRefund
      refundTotal = refundSubtotal + refundTaxTotal

      refundPayment =
        { id: dummyPaymentId
        , transactionId: refundId
        , method: Cash
        , amount: fromDiscrete' (negate refundTotal)
        , tendered: fromDiscrete' (negate refundTotal)
        , change: fromDiscrete' (Discrete 0)
        , reference: Nothing
        , approved: true
        , authorizationCode: Nothing
        }

      refundTransaction =
        { id: refundId
        , status: Completed
        , created: timestamp
        , completed: Just timestamp
        , customer: txData.customer
        , employee: employeeId
        , register: txData.register
        , location: txData.location
        , items: map makeRefundItem itemsToRefund
        , payments: [ PaymentTransaction refundPayment ]
        , subtotal: fromDiscrete' (negate refundSubtotal)
        , discountTotal: fromDiscrete' (Discrete 0)
        , taxTotal: fromDiscrete' (negate refundTaxTotal)
        , total: fromDiscrete' (negate refundTotal)
        , transactionType: Return
        , isVoided: false
        , voidReason: Nothing
        , isRefunded: false
        , refundReason: Just reason
        , referenceTransactionId: Just txId
        , notes: Just $ "Refund for transaction " <> uuidToString
            txId
        }

    liftEffect $ log $ "Refund processed: " <> uuidToString refundId
      <> ", Amount: "
      <> formatDiscrete numericC refundTotal

    pure $ Right (Transaction refundTransaction)
  where

  contains :: forall a. Array UUID -> UUID -> Boolean
  contains ids targetId =
    case Array.uncons ids of
      Nothing -> false
      Just { head, tail } ->
        if head == targetId then true
        else contains tail targetId

  makeRefundItem :: TransactionItem -> TransactionItem
  makeRefundItem (TransactionItem item) =
    TransactionItem $ item
      { transactionId = dummyTransactionId
      , subtotal = fromDiscrete' (negate (toDiscrete item.subtotal))
      , total = fromDiscrete' (negate (toDiscrete item.total))
      , taxes = map (\tax -> tax { amount = fromDiscrete' (negate (toDiscrete tax.amount)) }) item.taxes
      }

dummyAccountId :: UUID
dummyAccountId = UUID "00000000-0000-0000-0000-000000000001"

dummyPaymentId :: UUID
dummyPaymentId = UUID "00000000-0000-0000-0000-000000000003"

dummyTransactionId :: UUID
dummyTransactionId = UUID "00000000-0000-0000-0000-000000000002"