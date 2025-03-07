module Services.Ledger where

import Prelude
import Types.Transaction (Account, LedgerEntry(..), LedgerEntryType(..), LedgerError(..), Transaction)
import Utils.UUIDGen (genUUID)
import Data.Array (filter, foldl, null)
import Data.Array as Array
import Data.DateTime (DateTime)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..))
import Data.Finance.Currency (USD)
import Data.Finance.Money (Discrete(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now, nowDateTime)
import Types.DiscreteUSD (fromDiscrete, toDiscrete)
import Types.UUID (UUID(..)) 

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
  -- where
  --   -- Extract the record from the LedgerEntry newtype
  --   addEntry :: Discrete USD -> LedgerEntry -> Discrete USD
  --   addEntry acc entry = 
  --     let entryData = unwrap entry # \r -> r { amount = toDiscrete r.amount }
  --     in if entryData.isDebit
  --        then acc + entryData.amount
  --        else acc - entryData.amount

createSaleEntries ::
  Transaction ->
  UUID ->
  UUID ->
  UUID ->
  Aff (Either LedgerError (Array LedgerEntry))
createSaleEntries tx revenueAccount taxAccount assetAccount = do
  currentTime <- liftEffect now  -- Gets Instant
  
  saleEntryId <- liftEffect genUUID
  taxEntryId <- liftEffect genUUID
  paymentEntryId <- liftEffect genUUID

  let txData = unwrap tx

      saleEntry = LedgerEntry
        { id: saleEntryId
        , transactionId: txData.id
        , accountId: revenueAccount
        , amount: txData.subtotal
        , isDebit: false
        , timestamp: toDateTime currentTime 
        , entryType: SaleEntry
        , description: "Sale revenue"
        }

      taxEntry = LedgerEntry
        { id: taxEntryId
        , transactionId: txData.id
        , accountId: taxAccount
        , amount: txData.taxTotal
        , isDebit: false
        , timestamp: toDateTime currentTime
        , entryType: Tax
        , description: "Sales tax collected"
        }

      paymentEntry = LedgerEntry
        { id: paymentEntryId
        , transactionId: txData.id
        , accountId: assetAccount
        , amount: txData.total
        , isDebit: true
        , timestamp: toDateTime currentTime
        , entryType: Payment
        , description: "Customer payment"
        }

      entries = [saleEntry, taxEntry, paymentEntry]

  case validateLedgerBalance entries of
    Left err -> pure $ Left err
    Right _ -> do
      liftEffect $ log "Created balanced ledger entries for sale transaction"
      pure $ Right entries

createRefundEntries ::
  Transaction ->
  UUID ->
  UUID ->
  UUID ->
  Aff (Either LedgerError (Array LedgerEntry))
createRefundEntries tx revenueAccount taxAccount assetAccount = do
  timestamp <- liftEffect now
  saleEntryId <- liftEffect genUUID
  taxEntryId <- liftEffect genUUID
  paymentEntryId <- liftEffect genUUID

  let txData = unwrap tx

      saleEntry = LedgerEntry
        { id: saleEntryId
        , transactionId: txData.id
        , accountId: revenueAccount
        , amount: txData.subtotal
        , isDebit: true
        , timestamp: toDateTime timestamp
        , entryType: Refund
        , description: "Refund - revenue reversal"
        }

      taxEntry = LedgerEntry
        { id: taxEntryId
        , transactionId: txData.id
        , accountId: taxAccount
        , amount: txData.taxTotal
        , isDebit: true
        , timestamp: toDateTime timestamp
        , entryType: Refund
        , description: "Refund - tax reversal"
        }

      paymentEntry = LedgerEntry
        { id: paymentEntryId
        , transactionId: txData.id
        , accountId: assetAccount
        , amount: txData.total
        , isDebit: false
        , timestamp: toDateTime timestamp
        , entryType: Refund
        , description: "Customer refund"
        }

      entries = [saleEntry, taxEntry, paymentEntry]

  case validateLedgerBalance entries of
    Left err -> pure $ Left err
    Right _ -> do
      liftEffect $ log "Created balanced ledger entries for refund transaction"
      pure $ Right entries

getAccountBalance ::
  Array LedgerEntry ->
  UUID ->
  Maybe DateTime ->
  Discrete USD
getAccountBalance entries accountId maybeUntil =
  let
    accountEntries = filter (\entry -> (unwrap entry).accountId == accountId) entries

    timeFilteredEntries = case maybeUntil of
      Nothing -> accountEntries
      Just until -> filter (\entry -> (unwrap entry).timestamp <= until) accountEntries

    balance = foldl addEntry (Discrete 0) timeFilteredEntries
  in
    balance
  -- where
  --   addEntry :: Discrete USD -> LedgerEntry -> Discrete USD
  --   addEntry acc entry = 
  --     let entryData = unwrap entry # \r -> r { amount = toDiscrete r.amount }
  --     in if entryData.isDebit
  --        then acc + entryData.amount
  --        else acc - entryData.amount

createCashDrawerAdjustment ::
  UUID ->
  UUID ->
  Discrete USD ->
  Boolean ->
  String ->
  Maybe UUID ->
  Aff (Either LedgerError (Array LedgerEntry))
createCashDrawerAdjustment regId empId amt isAddition reason approver = do
  timestamp <- liftEffect now
  adjustmentEntryId <- liftEffect genUUID
  offsetEntryId <- liftEffect genUUID

  let
    cashAccountId = dummyAccountId
    overShortAccountId = dummyAccountId

    cashEntry = LedgerEntry
      { id: adjustmentEntryId
      , transactionId: dummyTransactionId
      , accountId: cashAccountId
      , amount: fromDiscrete amt
      , isDebit: isAddition
      , timestamp: toDateTime timestamp
      , entryType: Adjustment
      , description: "Cash drawer adjustment: " <> reason
      }

    offsetEntry = LedgerEntry
      { id: offsetEntryId
      , transactionId: dummyTransactionId
      , accountId: overShortAccountId
      , amount: fromDiscrete amt
      , isDebit: not isAddition
      , timestamp: toDateTime timestamp
      , entryType: Adjustment
      , description: "Cash drawer adjustment offset: " <> reason
      }

    entries = [cashEntry, offsetEntry]

  case validateLedgerBalance entries of
    Left err -> pure $ Left err
    Right _ -> do
      liftEffect $ log "Created balanced ledger entries for cash drawer adjustment"
      pure $ Right entries

calculateDailySales ::
  Array LedgerEntry ->
  DateTime ->
  DateTime ->
  Aff { cash :: Discrete USD, card :: Discrete USD, other :: Discrete USD, total :: Discrete USD }
calculateDailySales entries startTime endTime = do
  liftEffect $ log "Calculating daily sales totals"

  let
    dayEntries = filter (\entry -> 
                         let e = unwrap entry
                         in e.timestamp >= startTime &&
                            e.timestamp <= endTime &&
                            e.entryType == Payment)
                         entries

    cashEntries = filter (\_ -> true) dayEntries
    cardEntries = filter (\_ -> true) dayEntries
    otherEntries = filter (\_ -> true) dayEntries

    addAmount acc entry = 
      let entryData = unwrap entry
          amount = toDiscrete entryData.amount  -- Convert DiscreteUSD to Discrete USD
      in acc + amount
    
    cashTotal = foldl addAmount (Discrete 0) cashEntries
    cardTotal = foldl addAmount (Discrete 0) cardEntries
    otherTotal = foldl addAmount (Discrete 0) otherEntries
    totalSales = cashTotal + cardTotal + otherTotal

  pure {
    cash: cashTotal,
    card: cardTotal,
    other: otherTotal,
    total: totalSales
  }

generateLedgerReport ::
  Array Account ->
  Array LedgerEntry ->
  DateTime ->
  DateTime ->
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
        accountData = unwrap account
        openingBalance = getAccountBalance entries accountData.id (Just startDate)

        periodEntries = filter (\entry -> 
                               let e = unwrap entry
                               in e.accountId == accountData.id &&
                                  e.timestamp >= startDate &&
                                  e.timestamp <= endDate)
                               entries

        activity = foldl addEntry (Discrete 0) periodEntries

        closingBalance = openingBalance + activity
      in
        { account, openingBalance, activity, closingBalance }

    -- addEntry :: Discrete USD -> LedgerEntry -> Discrete USD
    -- addEntry acc entry = 
    --   let entryData = unwrap entry # \r -> r { amount = toDiscrete r.amount }
    --   in if entryData.isDebit
    --      then acc + entryData.amount
    --      else acc - entryData.amount

addEntry :: Discrete USD -> LedgerEntry -> Discrete USD
addEntry acc entry =
  let entryData = unwrap entry # \r -> r { amount = toDiscrete r.amount }
  in if entryData.isDebit
     then acc + entryData.amount
     else acc - entryData.amount

recordLedgerTransaction ::
  Transaction ->
  Array LedgerEntry ->
  Aff (Either LedgerError (Array LedgerEntry))
recordLedgerTransaction tx entries = do
  case validateLedgerBalance entries of
    Left err -> pure $ Left err
    Right _ -> do
      liftEffect $ log $ "Recording " <> show (length entries) <> " ledger entries for transaction " <> show (unwrap tx).id
      pure $ Right entries

length :: forall a. Array a -> Int
length = Array.length

dummyAccountId :: UUID
dummyAccountId = UUID "00000000-0000-0000-0000-000000000001"

dummyTransactionId :: UUID
dummyTransactionId = UUID "00000000-0000-0000-0000-000000000002"

dummyId :: UUID
dummyId = UUID "00000000-0000-0000-0000-000000000003"
