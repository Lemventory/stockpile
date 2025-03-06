module Ledger.System where

import Prelude
import Types.Transaction

import Data.Array (filter, foldl, null, (:))
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Finance.Currency (USD)
import Data.Finance.Money (Discrete(..))
import Data.Maybe (Maybe(..))
import Data.UUID (UUID, genUUID)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now)

-- | Represents possible errors in ledger operations
data LedgerError
  = UnbalancedTransaction
  | InvalidAccountReference
  | InsufficientFunds
  | DuplicateEntry
  | InvalidAmount
  | TransactionClosed
  | AuthorizationFailed
  | SystemError String

derive instance eqLedgerError :: Eq LedgerError
derive instance ordLedgerError :: Ord LedgerError

instance showLedgerError :: Show LedgerError where
  show UnbalancedTransaction = "Transaction debits and credits do not balance"
  show InvalidAccountReference = "Referenced account does not exist"
  show InsufficientFunds = "Insufficient funds in account"
  show DuplicateEntry = "Duplicate ledger entry detected"
  show InvalidAmount = "Invalid amount for ledger entry"
  show TransactionClosed = "Cannot modify a closed transaction"
  show AuthorizationFailed = "User not authorized for this operation"
  show (SystemError msg) = "System error: " <> msg

-- | Validates that a set of ledger entries balances (debits = credits)
validateLedgerBalance :: Array LedgerEntry -> Either LedgerError Unit
validateLedgerBalance entries =
  if null entries then
    Left UnbalancedTransaction
  else
    let
      sumEntries = foldl addEntry (Discrete 0) entries
    in
      if sumEntries == Discrete 0
        then Right unit
        else Left UnbalancedTransaction
  where
    addEntry :: Discrete USD -> LedgerEntry -> Discrete USD
    addEntry acc entry =
      if entry.isDebit
        then acc + entry.amount
        else acc - entry.amount

-- | Creates ledger entries for a sale transaction
createSaleEntries ::
  Transaction ->
  UUID -> -- Revenue account
  UUID -> -- Tax liability account
  UUID -> -- Asset (cash or receivable) account
  Aff (Either LedgerError (Array LedgerEntry))
createSaleEntries transaction revenueAccount taxAccount assetAccount = do
  -- Get current time
  timestamp <- liftEffect now
  
  -- Generate UUIDs for entries
  saleEntryId <- liftEffect genUUID
  taxEntryId <- liftEffect genUUID
  paymentEntryId <- liftEffect genUUID
  
  let
    -- Sale entry - CREDIT revenue account
    saleEntry =
      { id: saleEntryId
      , transactionId: transaction.id
      , accountId: revenueAccount
      , amount: transaction.subtotal
      , isDebit: false
      , timestamp
      , entryType: Sale
      , description: "Sale revenue"
      }
    
    -- Tax entry - CREDIT tax liability account
    taxEntry =
      { id: taxEntryId
      , transactionId: transaction.id
      , accountId: taxAccount
      , amount: transaction.taxTotal
      , isDebit: false
      , timestamp
      , entryType: Tax
      , description: "Sales tax collected"
      }
    
    -- Payment entry - DEBIT asset account
    paymentEntry =
      { id: paymentEntryId
      , transactionId: transaction.id
      , accountId: assetAccount
      , amount: transaction.total
      , isDebit: true
      , timestamp
      , entryType: Payment
      , description: "Customer payment"
      }
    
    entries = [saleEntry, taxEntry, paymentEntry]
  
  -- Validate that entries balance
  case validateLedgerBalance entries of
    Left err -> pure $ Left err
    Right _ -> do
      liftEffect $ log "Created balanced ledger entries for sale transaction"
      pure $ Right entries

-- | Creates ledger entries for a refund transaction
createRefundEntries ::
  Transaction ->
  UUID -> -- Revenue account
  UUID -> -- Tax liability account
  UUID -> -- Asset (cash or receivable) account
  Aff (Either LedgerError (Array LedgerEntry))
createRefundEntries transaction revenueAccount taxAccount assetAccount = do
  -- Get current time
  timestamp <- liftEffect now
  
  -- Generate UUIDs for entries
  saleEntryId <- liftEffect genUUID
  taxEntryId <- liftEffect genUUID
  paymentEntryId <- liftEffect genUUID
  
  let
    -- Reverse sale entry - DEBIT revenue account
    saleEntry =
      { id: saleEntryId
      , transactionId: transaction.id
      , accountId: revenueAccount
      , amount: transaction.subtotal
      , isDebit: true
      , timestamp
      , entryType: Refund
      , description: "Refund - revenue reversal"
      }
    
    -- Reverse tax entry - DEBIT tax liability account
    taxEntry =
      { id: taxEntryId
      , transactionId: transaction.id
      , accountId: taxAccount
      , amount: transaction.taxTotal
      , isDebit: true
      , timestamp
      , entryType: Refund
      , description: "Refund - tax reversal"
      }
    
    -- Payment return entry - CREDIT asset account
    paymentEntry =
      { id: paymentEntryId
      , transactionId: transaction.id
      , accountId: assetAccount
      , amount: transaction.total
      , isDebit: false
      , timestamp
      , entryType: Refund
      , description: "Customer refund"
      }
    
    entries = [saleEntry, taxEntry, paymentEntry]
  
  -- Validate that entries balance
  case validateLedgerBalance entries of
    Left err -> pure $ Left err
    Right _ -> do
      liftEffect $ log "Created balanced ledger entries for refund transaction"
      pure $ Right entries

-- | Calculate account balance up to a certain datetime
getAccountBalance ::
  Array LedgerEntry ->
  UUID ->
  Maybe DateTime ->
  Discrete USD
getAccountBalance entries accountId maybeUntil =
  let
    -- Filter entries for this account
    accountEntries = filter (\e -> e.accountId == accountId) entries
    
    -- Filter by date if specified
    timeFilteredEntries = case maybeUntil of
      Nothing -> accountEntries
      Just until -> filter (\e -> e.timestamp <= until) accountEntries
    
    -- Calculate balance
    balance = foldl addEntry (Discrete 0) timeFilteredEntries
  in
    balance
  where
    addEntry :: Discrete USD -> LedgerEntry -> Discrete USD
    addEntry acc entry =
      if entry.isDebit
        then acc + entry.amount
        else acc - entry.amount

-- | Create journal entries for a cash drawer adjustment
createCashDrawerAdjustment ::
  UUID -> -- Register ID
  UUID -> -- Employee ID
  Discrete USD -> -- Adjustment amount
  Boolean -> -- Is addition (true) or removal (false)
  String -> -- Reason
  Maybe UUID -> -- Approver (if required)
  Aff (Either LedgerError (Array LedgerEntry))
createCashDrawerAdjustment regId empId amount isAddition reason approver = do
  -- Get current time
  timestamp <- liftEffect now
  
  -- Generate UUIDs for entries
  adjustmentEntryId <- liftEffect genUUID
  offsetEntryId <- liftEffect genUUID
  
  liftEffect $ log $ "Creating cash drawer adjustment for register " <> show regId <>
               " by employee " <> show empId <>
               (case approver of
                 Just appId -> " (approved by " <> show appId <> ")"
                 Nothing -> "")
  
  -- These would come from a configuration or database in real implementation
  let
    cashAccountId = dummyAccountId -- Cash account
    overShortAccountId = dummyAccountId -- Over/short account
    
    -- First entry - affects cash account
    cashEntry =
      { id: adjustmentEntryId
      , transactionId: dummyTransactionId -- Would be linked to a real transaction
      , accountId: cashAccountId
      , amount
      , isDebit: isAddition -- Debit if adding cash, credit if removing
      , timestamp
      , entryType: Adjustment
      , description: "Cash drawer adjustment: " <> reason
      }
    
    -- Offsetting entry - affects over/short account
    offsetEntry =
      { id: offsetEntryId
      , transactionId: dummyTransactionId
      , accountId: overShortAccountId
      , amount
      , isDebit: not isAddition -- Opposite of cash entry
      , timestamp
      , entryType: Adjustment
      , description: "Cash drawer adjustment offset: " <> reason
      }
    
    entries = [cashEntry, offsetEntry]
  
  -- Validate that entries balance
  case validateLedgerBalance entries of
    Left err -> pure $ Left err
    Right _ -> do
      liftEffect $ log "Created balanced ledger entries for cash drawer adjustment"
      pure $ Right entries

-- | Calculate daily sales totals by payment method
calculateDailySales ::
  Array LedgerEntry ->
  DateTime -> -- Start of day
  DateTime -> -- End of day
  Aff { cash :: Discrete USD, card :: Discrete USD, other :: Discrete USD, total :: Discrete USD }
calculateDailySales entries startTime endTime = do
  liftEffect $ log "Calculating daily sales totals"
  
  -- This would actually filter entries from a database in a real implementation
  let
    dayEntries = filter (\entry ->
                           entry.timestamp >= startTime &&
                           entry.timestamp <= endTime &&
                           entry.entryType == Payment)
                         entries
    
    -- This is simplified - in a real system you'd look up payment methods
    -- These filters would use actual logic to determine payment type    
    cashEntries = filter (\_ -> true) dayEntries
    cardEntries = filter (\_ -> true) dayEntries 
    otherEntries = filter (\_ -> true) dayEntries 

    cashTotal = foldl (\acc e -> acc + e.amount) (Discrete 0) cashEntries
    cardTotal = foldl (\acc e -> acc + e.amount) (Discrete 0) cardEntries
    otherTotal = foldl (\acc e -> acc + e.amount) (Discrete 0) otherEntries
    totalSales = cashTotal + cardTotal + otherTotal
  
  pure {
    cash: cashTotal,
    card: cardTotal,
    other: otherTotal,
    total: totalSales
  }

-- | Generate a general ledger report
generateLedgerReport ::
  Array Account ->
  Array LedgerEntry ->
  DateTime -> -- Start date
  DateTime -> -- End date
  Aff (Array { account :: Account, openingBalance :: Discrete USD, activity :: Discrete USD, closingBalance :: Discrete USD })
generateLedgerReport accounts entries startDate endDate = do
  liftEffect $ log "Generating general ledger report"
  
  let
    results = map processAccount accounts
  
  pure results
  where
    processAccount :: Account -> { account :: Account, openingBalance :: Discrete USD, activity :: Discrete USD, closingBalance :: Discrete USD }
    processAccount account =
      let
        -- Opening balance - all entries before start date
        openingBalance = getAccountBalance entries account.id (Just startDate)
        
        -- Activity during period
        periodEntries = filter (\e -> 
                                 e.accountId == account.id && 
                                 e.timestamp >= startDate && 
                                 e.timestamp <= endDate) 
                               entries
        
        activity = foldl addEntry (Discrete 0) periodEntries
        
        -- Closing balance
        closingBalance = openingBalance + activity
      in
        { account, openingBalance, activity, closingBalance }
    
    addEntry :: Discrete USD -> LedgerEntry -> Discrete USD
    addEntry acc entry = 
      if entry.isDebit
        then acc + entry.amount
        else acc - entry.amount

-- | Record a new ledger transaction
recordLedgerTransaction :: 
  Transaction -> 
  Array LedgerEntry -> 
  Aff (Either LedgerError (Array LedgerEntry))
recordLedgerTransaction transaction entries = do
  -- Validate entries
  case validateLedgerBalance entries of
    Left err -> pure $ Left err
    Right _ -> do
      -- In a real implementation, this would save to a database
      liftEffect $ log $ "Recording " <> show (length entries) <> " ledger entries for transaction " <> show transaction.id
      pure $ Right entries

-- Helper function for array length (to avoid foreign import)
-- length :: forall a. Array a -> Int
-- length [] = 0
-- length (_ : xs) = 1 + length xs

length :: forall a. Array a -> Int
length arr = 
  if Array.null arr 
    then 0
    else 1 + length (Array.drop 1 arr)

-- Dummy values for example purposes only
dummyAccountId :: UUID
dummyAccountId = unsafeCoerce "00000000-0000-0000-0000-000000000001"

dummyTransactionId :: UUID
dummyTransactionId = unsafeCoerce "00000000-0000-0000-0000-000000000002"

-- | Unsafe coerce for example purposes only
foreign import unsafeCoerce :: forall a b. a -> b