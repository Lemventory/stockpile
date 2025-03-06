module DB.Transaction where

import Control.Exception (catch, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID, toString)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple
  ( Connection
  , Only(..)
  , Query
  , execute
  , execute_
  , fromOnly
  , query
  , query_
  , returning
  )
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Servant (Handler, err404, throwError)
import System.IO (hPutStrLn, stderr)

import Types.Transaction

type ConnectionPool = Pool Connection
type DBAction a = Connection -> IO a

-- | Helper to run a database action with a connection from the pool
withConnection :: ConnectionPool -> DBAction a -> IO a
withConnection = flip withResource

-- | Create transaction tables
createTransactionTables :: ConnectionPool -> IO ()
createTransactionTables pool = withConnection pool $ \conn -> do
  -- Execute the SQL to create transaction tables
  -- Note: This would use the SQL from our first artifact
  hPutStrLn stderr "Creating transaction tables..."
  do
    results <- execute_ conn [sql|
      SELECT 1 FROM information_schema.tables
      WHERE table_name = 'transaction'
    |]
    case results of
    [] -> do
      -- Tables don't exist, so create them
      hPutStrLn stderr "Transaction tables not found, creating..."
      -- Include the SQL from the first artifact here
      -- For brevity, we're not including the full SQL here
      pure ()
    _ -> do
      -- Tables already exist
      hPutStrLn stderr "Transaction tables already exist"
      pure ()

-- Transaction Functions --

-- | Get all transactions
getAllTransactions :: ConnectionPool -> IO [Transaction]
getAllTransactions pool = withConnection pool $ \conn ->
  query_ conn [sql|
    SELECT 
      t.id, 
      t.status, 
      t.created, 
      t.completed, 
      t.customer_id, 
      t.employee_id, 
      t.register_id, 
      t.location_id,
      t.subtotal, 
      t.discount_total, 
      t.tax_total, 
      t.total, 
      t.transaction_type, 
      t.is_voided, 
      t.void_reason, 
      t.is_refunded, 
      t.refund_reason, 
      t.reference_transaction_id, 
      t.notes
    FROM transaction t
    ORDER BY t.created DESC
  |]

-- | Get transaction by ID
getTransactionById :: ConnectionPool -> UUID -> IO (Maybe Transaction)
getTransactionById pool transactionId = withConnection pool $ \conn -> do
  results <- query conn [sql|
    SELECT 
      t.id, 
      t.status, 
      t.created, 
      t.completed, 
      t.customer_id, 
      t.employee_id, 
      t.register_id, 
      t.location_id,
      t.subtotal, 
      t.discount_total, 
      t.tax_total, 
      t.total, 
      t.transaction_type, 
      t.is_voided, 
      t.void_reason, 
      t.is_refunded, 
      t.refund_reason, 
      t.reference_transaction_id, 
      t.notes
    FROM transaction t
    WHERE t.id = ?
  |] (Only transactionId)
  
  case results of
    [transaction] -> do
      -- Get transaction items
      items <- getTransactionItemsByTransactionId conn transactionId
      
      -- Get transaction payments
      payments <- getPaymentsByTransactionId conn transactionId
      
      -- Return the complete transaction
      pure $ Just $ transaction { transactionItems = items, transactionPayments = payments }
    
    _ -> pure Nothing

-- | Get transaction items by transaction ID
getTransactionItemsByTransactionId :: Connection -> UUID -> IO [TransactionItem]
getTransactionItemsByTransactionId conn transactionId = do
  -- Get base transaction items
  items <- query conn [sql|
    SELECT 
      id, 
      transaction_id, 
      menu_item_sku, 
      quantity, 
      price_per_unit,
      subtotal, 
      total
    FROM transaction_item
    WHERE transaction_id = ?
  |] (Only transactionId)
  
  -- For each item, get discounts and taxes
  mapM (\item -> do
    discounts <- getDiscountsByTransactionItemId conn (transactionItemId item)
    taxes <- getTaxesByTransactionItemId conn (transactionItemId item)
    pure $ item { transactionItemDiscounts = discounts, transactionItemTaxes = taxes }
  ) items

-- | Get discounts by transaction item ID
getDiscountsByTransactionItemId :: Connection -> UUID -> IO [DiscountRecord]
getDiscountsByTransactionItemId conn itemId =
  query conn [sql|
    SELECT 
      d.type, 
      d.amount, 
      d.percent, 
      d.reason, 
      d.approved_by
    FROM discount d
    WHERE d.transaction_item_id = ?
  |] (Only itemId)

-- | Get taxes by transaction item ID
getTaxesByTransactionItemId :: Connection -> UUID -> IO [TaxRecord]
getTaxesByTransactionItemId conn itemId =
  query conn [sql|
    SELECT 
      t.category, 
      t.rate, 
      t.amount, 
      t.description
    FROM transaction_tax t
    WHERE t.transaction_item_id = ?
  |] (Only itemId)

-- | Get payment transactions by transaction ID
getPaymentsByTransactionId :: Connection -> UUID -> IO [PaymentTransaction]
getPaymentsByTransactionId conn transactionId =
  query conn [sql|
    SELECT 
      id, 
      transaction_id, 
      method, 
      amount, 
      tendered, 
      change_amount, 
      reference, 
      approved, 
      authorization_code
    FROM payment_transaction
    WHERE transaction_id = ?
  |] (Only transactionId)

-- | Create a new transaction
createTransaction :: ConnectionPool -> Transaction -> IO Transaction
createTransaction pool transaction = withConnection pool $ \conn -> do
  -- Insert transaction
  [newTransaction] <- query conn [sql|
    INSERT INTO transaction (
      id, 
      status, 
      created, 
      completed, 
      customer_id, 
      employee_id, 
      register_id, 
      location_id,
      subtotal, 
      discount_total, 
      tax_total, 
      total, 
      transaction_type, 
      is_voided, 
      void_reason, 
      is_refunded, 
      refund_reason, 
      reference_transaction_id, 
      notes
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    RETURNING 
      id, 
      status, 
      created, 
      completed, 
      customer_id, 
      employee_id, 
      register_id, 
      location_id,
      subtotal, 
      discount_total, 
      tax_total, 
      total, 
      transaction_type, 
      is_voided, 
      void_reason, 
      is_refunded, 
      refund_reason, 
      reference_transaction_id, 
      notes
  |] (
    transactionId transaction,
    showStatus (transactionStatus transaction),
    transactionCreated transaction,
    transactionCompleted transaction,
    transactionCustomerId transaction,
    transactionEmployeeId transaction,
    transactionRegisterId transaction,
    transactionLocationId transaction,
    transactionSubtotal transaction,
    transactionDiscountTotal transaction,
    transactionTaxTotal transaction,
    transactionTotal transaction,
    showTransactionType (transactionType transaction),
    transactionIsVoided transaction,
    transactionVoidReason transaction,
    transactionIsRefunded transaction,
    transactionRefundReason transaction,
    transactionReferenceTransactionId transaction,
    transactionNotes transaction
  )
  
  -- Insert transaction items
  newItems <- mapM (insertTransactionItem conn) (transactionItems transaction)
  
  -- Insert payment transactions
  newPayments <- mapM (insertPaymentTransaction conn) (transactionPayments transaction)
  
  -- Return the complete transaction
  pure $ newTransaction { transactionItems = newItems, transactionPayments = newPayments }

-- | Insert a transaction item
insertTransactionItem :: Connection -> TransactionItem -> IO TransactionItem
insertTransactionItem conn item = do
  -- Insert transaction item
  [newItem] <- query conn [sql|
    INSERT INTO transaction_item (
      id, 
      transaction_id, 
      menu_item_sku, 
      quantity, 
      price_per_unit, 
      subtotal, 
      total
    ) VALUES (?, ?, ?, ?, ?, ?, ?)
    RETURNING 
      id, 
      transaction_id, 
      menu_item_sku, 
      quantity, 
      price_per_unit, 
      subtotal, 
      total
  |] (
    transactionItemId item,
    transactionItemTransactionId item,
    transactionItemMenuItemSku item,
    transactionItemQuantity item,
    transactionItemPricePerUnit item,
    transactionItemSubtotal item,
    transactionItemTotal item
  )
  
  -- Insert discounts
  discounts <- mapM (insertDiscount conn (transactionItemId item) Nothing) 
                   (transactionItemDiscounts item)
  
  -- Insert taxes
  taxes <- mapM (insertTax conn (transactionItemId item)) 
               (transactionItemTaxes item)
  
  -- Return complete item
  pure $ newItem { transactionItemDiscounts = discounts, transactionItemTaxes = taxes }

-- | Insert a discount
insertDiscount :: Connection -> UUID -> Maybe UUID -> DiscountRecord -> IO DiscountRecord
insertDiscount conn itemId transactionId discount = do
  discountId <- liftIO nextRandom
  execute conn [sql|
    INSERT INTO discount (
      id,
      transaction_item_id,
      transaction_id,
      type,
      amount,
      percent,
      reason,
      approved_by
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
  |] (
    discountId,
    itemId,
    transactionId,
    showDiscountType (discountType discount),
    discountAmount discount,
    getDiscountPercent (discountType discount),
    discountReason discount,
    discountApprovedBy discount
  )
  pure discount

-- | Get percent from discount type
getDiscountPercent :: DiscountType -> Maybe Scientific
getDiscountPercent (PercentOff percent) = Just percent
getDiscountPercent _ = Nothing

-- | Insert a tax record
insertTax :: Connection -> UUID -> TaxRecord -> IO TaxRecord
insertTax conn itemId tax = do
  taxId <- liftIO nextRandom
  execute conn [sql|
    INSERT INTO transaction_tax (
      id,
      transaction_item_id,
      category,
      rate,
      amount,
      description
    ) VALUES (?, ?, ?, ?, ?, ?)
  |] (
    taxId,
    itemId,
    showTaxCategory (taxCategory tax),
    taxRate tax,
    taxAmount tax,
    taxDescription tax
  )
  pure tax

-- | Insert a payment transaction
insertPaymentTransaction :: Connection -> PaymentTransaction -> IO PaymentTransaction
insertPaymentTransaction conn payment = do
  [newPayment] <- query conn [sql|
    INSERT INTO payment_transaction (
      id,
      transaction_id,
      method,
      amount,
      tendered,
      change_amount,
      reference,
      approved,
      authorization_code
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    RETURNING
      id,
      transaction_id,
      method,
      amount,
      tendered,
      change_amount,
      reference,
      approved,
      authorization_code
  |] (
    paymentId payment,
    paymentTransactionId payment,
    showPaymentMethod (paymentMethod payment),
    paymentAmount payment,
    paymentTendered payment,
    paymentChange payment,
    paymentReference payment,
    paymentApproved payment,
    paymentAuthorizationCode payment
  )
  pure newPayment

-- | Update a transaction
updateTransaction :: ConnectionPool -> UUID -> Transaction -> IO Transaction
updateTransaction pool transactionId transaction = withConnection pool $ \conn -> do
  -- Update transaction
  execute conn [sql|
    UPDATE transaction SET
      status = ?,
      completed = ?,
      customer_id = ?,
      employee_id = ?,
      register_id = ?,
      location_id = ?,
      subtotal = ?,
      discount_total = ?,
      tax_total = ?,
      total = ?,
      transaction_type = ?,
      is_voided = ?,
      void_reason = ?,
      is_refunded = ?,
      refund_reason = ?,
      reference_transaction_id = ?,
      notes = ?
    WHERE id = ?
  |] (
    showStatus (transactionStatus transaction),
    transactionCompleted transaction,
    transactionCustomerId transaction,
    transactionEmployeeId transaction,
    transactionRegisterId transaction,
    transactionLocationId transaction,
    transactionSubtotal transaction,
    transactionDiscountTotal transaction,
    transactionTaxTotal transaction,
    transactionTotal transaction,
    showTransactionType (transactionType transaction),
    transactionIsVoided transaction,
    transactionVoidReason transaction,
    transactionIsRefunded transaction,
    transactionRefundReason transaction,
    transactionReferenceTransactionId transaction,
    transactionNotes transaction,
    transactionId
  )
  
  -- Get the updated transaction
  maybeTransaction <- getTransactionById pool transactionId
  case maybeTransaction of
    Just updatedTransaction -> pure updatedTransaction
    Nothing -> error $ "Transaction not found after update: " ++ show transactionId

-- | Void a transaction
voidTransaction :: ConnectionPool -> UUID -> Text -> IO Transaction
voidTransaction pool transactionId reason = withConnection pool $ \conn -> do
  -- Update transaction to voided status
  execute conn [sql|
    UPDATE transaction SET
      status = 'VOIDED',
      is_voided = TRUE,
      void_reason = ?
    WHERE id = ?
  |] (reason, transactionId)
  
  -- Get the updated transaction
  maybeTransaction <- getTransactionById pool transactionId
  case maybeTransaction of
    Just updatedTransaction -> pure updatedTransaction
    Nothing -> error $ "Transaction not found after void: " ++ show transactionId

-- | Refund a transaction
refundTransaction :: ConnectionPool -> UUID -> Text -> IO Transaction
refundTransaction pool transactionId reason = withConnection pool $ \conn -> do
  -- First, get the original transaction
  maybeOriginalTransaction <- getTransactionById pool transactionId
  case maybeOriginalTransaction of
    Nothing -> error $ "Original transaction not found for refund: " ++ show transactionId
    Just originalTransaction -> do
      -- Create a new transaction for the refund
      refundId <- liftIO nextRandom
      now <- liftIO getCurrentTime
      
      let refundTransaction = Transaction {
        transactionId = refundId,
        transactionStatus = Completed,
        transactionCreated = now,
        transactionCompleted = Just now,
        transactionCustomerId = transactionCustomerId originalTransaction,
        transactionEmployeeId = transactionEmployeeId originalTransaction,
        transactionRegisterId = transactionRegisterId originalTransaction,
        transactionLocationId = transactionLocationId originalTransaction,
        -- Negate amounts for refund
        transactionSubtotal = negate $ transactionSubtotal originalTransaction,
        transactionDiscountTotal = negate $ transactionDiscountTotal originalTransaction,
        transactionTaxTotal = negate $ transactionTaxTotal originalTransaction,
        transactionTotal = negate $ transactionTotal originalTransaction,
        transactionType = Return,
        transactionIsVoided = False,
        transactionVoidReason = Nothing,
        transactionIsRefunded = False,
        transactionRefundReason = Nothing,
        transactionReferenceTransactionId = Just transactionId,
        transactionNotes = Just $ "Refund for transaction " <> T.pack (toString transactionId) <> ": " <> reason,
        -- Create refund items and payment
        transactionItems = map negateTransactionItem $ transactionItems originalTransaction,
        transactionPayments = map negatePaymentTransaction $ transactionPayments originalTransaction
      }
      
      -- Insert the refund transaction
      newRefundTransaction <- createTransaction pool refundTransaction
      
      -- Mark the original transaction as refunded
      execute conn [sql|
        UPDATE transaction SET
          is_refunded = TRUE,
          refund_reason = ?
        WHERE id = ?
      |] (reason, transactionId)
      
      -- Return the refund transaction
      pure newRefundTransaction

-- | Negate a transaction item for refunds
negateTransactionItem :: TransactionItem -> TransactionItem
negateTransactionItem item = item {
  transactionItemId = item.transactionItemId, -- Will be replaced when inserted
  transactionItemSubtotal = negate $ transactionItemSubtotal item,
  transactionItemTotal = negate $ transactionItemTotal item,
  transactionItemDiscounts = map negateDiscountRecord $ transactionItemDiscounts item,
  transactionItemTaxes = map negateTaxRecord $ transactionItemTaxes item
}

-- | Negate a discount record for refunds
negateDiscountRecord :: DiscountRecord -> DiscountRecord
negateDiscountRecord discount = discount {
  discountAmount = negate $ discountAmount discount
}

-- | Negate a tax record for refunds
negateTaxRecord :: TaxRecord -> TaxRecord
negateTaxRecord tax = tax {
  taxAmount = negate $ taxAmount tax
}

-- | Negate a payment transaction for refunds
negatePaymentTransaction :: PaymentTransaction -> PaymentTransaction
negatePaymentTransaction payment = payment {
  paymentId = paymentId payment, -- Will be replaced when inserted
  paymentAmount = negate $ paymentAmount payment,
  paymentTendered = negate $ paymentTendered payment,
  paymentChange = negate $ paymentChange payment
}

-- | Finalize a transaction
finalizeTransaction :: ConnectionPool -> UUID -> IO Transaction
finalizeTransaction pool transactionId = withConnection pool $ \conn -> do
  -- Update transaction status to Completed
  now <- liftIO getCurrentTime
  execute conn [sql|
    UPDATE transaction SET
      status = 'COMPLETED',
      completed = ?
    WHERE id = ?
  |] (now, transactionId)
  
  -- Get the updated transaction
  maybeTransaction <- getTransactionById pool transactionId
  case maybeTransaction of
    Just updatedTransaction -> pure updatedTransaction
    Nothing -> error $ "Transaction not found after finalization: " ++ show transactionId

-- | Add an item to a transaction
addTransactionItem :: ConnectionPool -> TransactionItem -> IO TransactionItem
addTransactionItem pool item = withConnection pool $ \conn -> do
  -- Insert the transaction item
  newItem <- insertTransactionItem conn item
  
  -- Update transaction totals
  updateTransactionTotals conn (transactionItemTransactionId item)
  
  -- Return the new item
  pure newItem

-- | Delete a transaction item
deleteTransactionItem :: ConnectionPool -> UUID -> IO ()
deleteTransactionItem pool itemId = withConnection pool $ \conn -> do
  -- Get transaction ID before deleting
  results <- query conn [sql|
    SELECT transaction_id FROM transaction_item WHERE id = ?
  |] (Only itemId)
  
  case results of
    [Only transactionId] -> do
      -- Delete discounts for this item
      execute conn [sql|
        DELETE FROM discount WHERE transaction_item_id = ?
      |] (Only itemId)
      
      -- Delete taxes for this item
      execute conn [sql|
        DELETE FROM transaction_tax WHERE transaction_item_id = ?
      |] (Only itemId)
      
      -- Delete the item
      execute conn [sql|
        DELETE FROM transaction_item WHERE id = ?
      |] (Only itemId)
      
      -- Update transaction totals
      updateTransactionTotals conn transactionId
      
    _ -> pure () -- Item not found

-- | Add a payment to a transaction
addPaymentTransaction :: ConnectionPool -> PaymentTransaction -> IO PaymentTransaction
addPaymentTransaction pool payment = withConnection pool $ \conn -> do
  -- Insert the payment
  newPayment <- insertPaymentTransaction conn payment
  
  -- Update transaction
  updateTransactionPaymentStatus conn (paymentTransactionId payment)
  
  -- Return the new payment
  pure newPayment

-- | Delete a payment
deletePaymentTransaction :: ConnectionPool -> UUID -> IO ()
deletePaymentTransaction pool paymentId = withConnection pool $ \conn -> do
  -- Get transaction ID before deleting
  results <- query conn [sql|
    SELECT transaction_id FROM payment_transaction WHERE id = ?
  |] (Only paymentId)
  
  case results of
    [Only transactionId] -> do
      -- Delete the payment
      execute conn [sql|
        DELETE FROM payment_transaction WHERE id = ?
      |] (Only paymentId)
      
      -- Update transaction status
      updateTransactionPaymentStatus conn transactionId
      
    _ -> pure () -- Payment not found

-- | Update transaction totals
updateTransactionTotals :: Connection -> UUID -> IO ()
updateTransactionTotals conn transactionId = do
  -- Calculate subtotal from items
  [Only subtotal] <- query conn [sql|
    SELECT COALESCE(SUM(subtotal), 0) FROM transaction_item WHERE transaction_id = ?
  |] (Only transactionId)
  
  -- Calculate discount total from both transaction and item discounts
  [Only discountTotal] <- query conn [sql|
    SELECT COALESCE(SUM(amount), 0) FROM discount 
    WHERE transaction_id = ? OR transaction_item_id IN (
      SELECT id FROM transaction_item WHERE transaction_id = ?
    )
  |] (transactionId, transactionId)
  
  -- Calculate tax total from item taxes
  [Only taxTotal] <- query conn [sql|
    SELECT COALESCE(SUM(amount), 0) FROM transaction_tax 
    WHERE transaction_item_id IN (
      SELECT id FROM transaction_item WHERE transaction_id = ?
    )
  |] (Only transactionId)
  
  -- Calculate total
  let total = subtotal - discountTotal + taxTotal
  
  -- Update transaction
  execute conn [sql|
    UPDATE transaction SET
      subtotal = ?,
      discount_total = ?,
      tax_total = ?,
      total = ?
    WHERE id = ?
  |] (subtotal, discountTotal, taxTotal, total, transactionId)

-- | Update transaction status based on payments
updateTransactionPaymentStatus :: Connection -> UUID -> IO ()
updateTransactionPaymentStatus conn transactionId = do
  -- Get transaction total
  [Only total] <- query conn [sql|
    SELECT total FROM transaction WHERE id = ?
  |] (Only transactionId)
  
  -- Get payment total
  [Only paymentTotal] <- query conn [sql|
    SELECT COALESCE(SUM(amount), 0) FROM payment_transaction 
    WHERE transaction_id = ?
  |] (Only transactionId)
  
  -- Determine status based on payment
  let status = if paymentTotal >= total then "COMPLETED" else "IN_PROGRESS"
  
  -- Update transaction status
  execute conn [sql|
    UPDATE transaction SET
      status = ?
    WHERE id = ?
  |] (status, transactionId)

-- Helper functions to convert enum types to database strings
showStatus :: TransactionStatus -> Text
showStatus Created = "CREATED"
showStatus InProgress = "IN_PROGRESS"
showStatus Completed = "COMPLETED"
showStatus Voided = "VOIDED"
showStatus Refunded = "REFUNDED"

showTransactionType :: TransactionType -> Text
showTransactionType Sale = "SALE"
showTransactionType Return = "RETURN"
showTransactionType Exchange = "EXCHANGE"
showTransactionType InventoryAdjustment = "INVENTORY_ADJUSTMENT"
showTransactionType ManagerComp = "MANAGER_COMP"
showTransactionType Administrative = "ADMINISTRATIVE"

showPaymentMethod :: PaymentMethod -> Text
showPaymentMethod Cash = "CASH"
showPaymentMethod Debit = "DEBIT"
showPaymentMethod Credit = "CREDIT"
showPaymentMethod ACH = "ACH"
showPaymentMethod GiftCard = "GIFT_CARD"
showPaymentMethod StoredValue = "STORED_VALUE"
showPaymentMethod Mixed = "MIXED"
showPaymentMethod (Other text) = "OTHER"

showTaxCategory :: TaxCategory -> Text
showTaxCategory RegularSalesTax = "REGULAR_SALES_TAX"
showTaxCategory ExciseTax = "EXCISE_TAX"
showTaxCategory CannabisTax = "CANNABIS_TAX"
showTaxCategory LocalTax = "LOCAL_TAX"
showTaxCategory MedicalTax = "MEDICAL_TAX"
showTaxCategory NoTax = "NO_TAX"

showDiscountType :: DiscountType -> Text
showDiscountType (PercentOff _) = "PERCENT_OFF"
showDiscountType (AmountOff _) = "AMOUNT_OFF"
showDiscountType BuyOneGetOne = "BUY_ONE_GET_ONE"
showDiscountType (Custom _ _) = "CUSTOM"

-- Register functions --

-- | Get all registers
getAllRegisters :: ConnectionPool -> IO [Register]
getAllRegisters pool = withConnection pool $ \conn ->
  query_ conn [sql|
    SELECT 
      id, 
      name, 
      location_id, 
      is_open, 
      current_drawer_amount, 
      expected_drawer_amount,
      opened_at, 
      opened_by, 
      last_transaction_time
    FROM register
    ORDER BY name
  |]

-- | Get register by ID
getRegisterById :: ConnectionPool -> UUID -> IO (Maybe Register)
getRegisterById pool registerId = withConnection pool $ \conn -> do
  results <- query conn [sql|
    SELECT 
      id, 
      name, 
      location_id, 
      is_open, 
      current_drawer_amount, 
      expected_drawer_amount,
      opened_at, 
      opened_by, 
      last_transaction_time
    FROM register
    WHERE id = ?
  |] (Only registerId)
  
  case results of
    [register] -> pure $ Just register
    _ -> pure Nothing

-- | Create a register
createRegister :: ConnectionPool -> Register -> IO Register
createRegister pool register = withConnection pool $ \conn ->
  head <$> query conn [sql|
    INSERT INTO register (
      id, 
      name, 
      location_id, 
      is_open, 
      current_drawer_amount, 
      expected_drawer_amount,
      opened_at, 
      opened_by, 
      last_transaction_time
    ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    RETURNING 
      id, 
      name, 
      location_id, 
      is_open, 
      current_drawer_amount, 
      expected_drawer_amount,
      opened_at, 
      opened_by, 
      last_transaction_time
  |] (
    registerId register,
    registerName register,
    registerLocationId register,
    registerIsOpen register,
    registerCurrentDrawerAmount register,
    registerExpectedDrawerAmount register,
    registerOpenedAt register,
    registerOpenedBy register,
    registerLastTransactionTime register
  )

-- | Update a register
updateRegister :: ConnectionPool -> UUID -> Register -> IO Register
updateRegister pool registerId register = withConnection pool $ \conn -> do
  execute conn [sql|
    UPDATE register SET
      name = ?,
      location_id = ?,
      is_open = ?,
      current_drawer_amount = ?,
      expected_drawer_amount = ?,
      opened_at = ?,
      opened_by = ?,
      last_transaction_time = ?
    WHERE id = ?
  |] (
    registerName register,
    registerLocationId register,
    registerIsOpen register,
    registerCurrentDrawerAmount register,
    registerExpectedDrawerAmount register,
    registerOpenedAt register,
    registerOpenedBy register,
    registerLastTransactionTime register,
    registerId
  )
  
  -- Get the updated register
  maybeRegister <- getRegisterById pool registerId
  case maybeRegister of
    Just updatedRegister -> pure updatedRegister
    Nothing -> error $ "Register not found after update: " ++ show registerId

-- | Open a register
openRegister :: ConnectionPool -> UUID -> OpenRegisterRequest -> IO Register
openRegister pool registerId request = withConnection pool $ \conn -> do
  now <- liftIO getCurrentTime
  
  -- Update register to open status
  execute conn [sql|
    UPDATE register SET
      is_open = TRUE,
      current_drawer_amount = ?,
      expected_drawer_amount = ?,
      opened_at = ?,
      opened_by = ?
    WHERE id = ?
  |] (
    openRegisterStartingCash request,
    openRegisterStartingCash request,
    now,
    openRegisterEmployeeId request,
    registerId
  )
  
  -- Get the updated register
  maybeRegister <- getRegisterById pool registerId
  case maybeRegister of
    Just updatedRegister -> pure updatedRegister
    Nothing -> error $ "Register not found after opening: " ++ show registerId

-- | Close a register
closeRegister :: ConnectionPool -> UUID -> CloseRegisterRequest -> IO CloseRegisterResult
closeRegister pool registerId request = withConnection pool $ \conn -> do
  now <- liftIO getCurrentTime
  
  -- Get the current register
  maybeRegister <- getRegisterById pool registerId
  case maybeRegister of
    Nothing -> error $ "Register not found: " ++ show registerId
    Just register -> do
      -- Calculate variance
      let variance = registerExpectedDrawerAmount register - closeRegisterCountedCash request
      
      -- Update register to closed status
      execute conn [sql|
        UPDATE register SET
          is_open = FALSE,
          current_drawer_amount = ?,
          last_transaction_time = ?
        WHERE id = ?
      |] (
        closeRegisterCountedCash request,
        now,
        registerId
      )
      
      -- Get the updated register
      maybeUpdatedRegister <- getRegisterById pool registerId
      case maybeUpdatedRegister of
        Nothing -> error $ "Register not found after closing: " ++ show registerId
        Just updatedRegister -> 
          pure $ CloseRegisterResult {
            closeRegisterResultRegister = updatedRegister,
            closeRegisterResultVariance = variance
          }