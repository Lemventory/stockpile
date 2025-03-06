{-# LANGUAGE DeriveGeneric #-}
module Types.Transaction where

import Data.UUID (UUID)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Scientific (Scientific)
import Data.Aeson (ToJSON(..), FromJSON(..))
import GHC.Generics

-- | Transaction status enumeration
data TransactionStatus
  = Created
  | InProgress
  | Completed
  | Voided
  | Refunded
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TransactionStatus
instance FromJSON TransactionStatus

-- | Transaction type enumeration
data TransactionType
  = Sale
  | Return
  | Exchange
  | InventoryAdjustment
  | ManagerComp
  | Administrative
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TransactionType
instance FromJSON TransactionType

-- | Payment method enumeration
data PaymentMethod
  = Cash
  | Debit
  | Credit
  | ACH
  | GiftCard
  | StoredValue
  | Mixed
  | Other Text
  deriving (Show, Eq, Ord, Generic)

instance ToJSON PaymentMethod
instance FromJSON PaymentMethod

-- | Tax category enumeration
data TaxCategory
  = RegularSalesTax
  | ExciseTax
  | CannabisTax
  | LocalTax
  | MedicalTax
  | NoTax
  deriving (Show, Eq, Ord, Generic)

instance ToJSON TaxCategory
instance FromJSON TaxCategory

-- | Discount type enumeration
data DiscountType
  = PercentOff Scientific
  | AmountOff Scientific
  | BuyOneGetOne
  | Custom Text Scientific
  deriving (Show, Eq, Ord, Generic)

instance ToJSON DiscountType
instance FromJSON DiscountType

-- | Tax record
data TaxRecord = TaxRecord
  { taxCategory :: TaxCategory
  , taxRate :: Scientific
  , taxAmount :: Scientific
  , taxDescription :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON TaxRecord
instance FromJSON TaxRecord

-- | Discount record
data DiscountRecord = DiscountRecord
  { discountType :: DiscountType
  , discountAmount :: Scientific
  , discountReason :: Text
  , discountApprovedBy :: Maybe UUID
  } deriving (Show, Eq, Generic)

instance ToJSON DiscountRecord
instance FromJSON DiscountRecord

-- | Transaction item
data TransactionItem = TransactionItem
  { transactionItemId :: UUID
  , transactionItemTransactionId :: UUID
  , transactionItemMenuItemSku :: UUID
  , transactionItemQuantity :: Scientific
  , transactionItemPricePerUnit :: Scientific
  , transactionItemDiscounts :: [DiscountRecord]
  , transactionItemTaxes :: [TaxRecord]
  , transactionItemSubtotal :: Scientific
  , transactionItemTotal :: Scientific
  } deriving (Show, Eq, Generic)

instance ToJSON TransactionItem
instance FromJSON TransactionItem

-- | Payment transaction
data PaymentTransaction = PaymentTransaction
  { paymentId :: UUID
  , paymentTransactionId :: UUID
  , paymentMethod :: PaymentMethod
  , paymentAmount :: Scientific
  , paymentTendered :: Scientific
  , paymentChange :: Scientific
  , paymentReference :: Maybe Text
  , paymentApproved :: Bool
  , paymentAuthorizationCode :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON PaymentTransaction
instance FromJSON PaymentTransaction

-- | Transaction
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
  , transactionSubtotal :: Scientific
  , transactionDiscountTotal :: Scientific
  , transactionTaxTotal :: Scientific
  , transactionTotal :: Scientific
  , transactionType :: TransactionType
  , transactionIsVoided :: Bool
  , transactionVoidReason :: Maybe Text
  , transactionIsRefunded :: Bool
  , transactionRefundReason :: Maybe Text
  , transactionReferenceTransactionId :: Maybe UUID
  , transactionNotes :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON Transaction
instance FromJSON Transaction

-- | Ledger entry type enumeration
data LedgerEntryType
  = SaleEntry
  | Tax
  | Discount
  | Payment
  | Refund
  | Void
  | Adjustment
  | Fee
  deriving (Show, Eq, Ord, Generic)

instance ToJSON LedgerEntryType
instance FromJSON LedgerEntryType

-- | Account type enumeration
data AccountType
  = Asset
  | Liability
  | Equity
  | Revenue
  | Expense
  deriving (Show, Eq, Ord, Generic)

instance ToJSON AccountType
instance FromJSON AccountType

-- | Account
data Account = Account
  { accountId :: UUID
  , accountCode :: Text
  , accountName :: Text
  , accountIsDebitNormal :: Bool
  , accountParentAccountId :: Maybe UUID
  , accountType :: AccountType
  } deriving (Show, Eq, Generic)

instance ToJSON Account
instance FromJSON Account

-- | Ledger entry
data LedgerEntry = LedgerEntry
  { ledgerEntryId :: UUID
  , ledgerEntryTransactionId :: UUID
  , ledgerEntryAccountId :: UUID
  , ledgerEntryAmount :: Scientific
  , ledgerEntryIsDebit :: Bool
  , ledgerEntryTimestamp :: UTCTime
  , ledgerEntryType :: LedgerEntryType
  , ledgerEntryDescription :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON LedgerEntry
instance FromJSON LedgerEntry

-- | Verification type enumeration
data VerificationType
  = AgeVerification
  | MedicalCardVerification
  | IDScan
  | VisualInspection
  | PatientRegistration
  | PurchaseLimitCheck
  deriving (Show, Eq, Ord, Generic)

instance ToJSON VerificationType
instance FromJSON VerificationType

-- | Verification status enumeration
data VerificationStatus
  = VerifiedStatus
  | FailedStatus
  | ExpiredStatus
  | NotRequiredStatus
  deriving (Show, Eq, Ord, Generic)

instance ToJSON VerificationStatus
instance FromJSON VerificationStatus

-- | Customer verification
data CustomerVerification = CustomerVerification
  { customerVerificationId :: UUID
  , customerVerificationCustomerId :: UUID
  , customerVerificationType :: VerificationType
  , customerVerificationStatus :: VerificationStatus
  , customerVerificationVerifiedBy :: UUID
  , customerVerificationVerifiedAt :: UTCTime
  , customerVerificationExpiresAt :: Maybe UTCTime
  , customerVerificationNotes :: Maybe Text
  , customerVerificationDocumentId :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON CustomerVerification
instance FromJSON CustomerVerification

-- | Reporting status enumeration
data ReportingStatus
  = NotRequired
  | Pending
  | Submitted
  | Acknowledged
  | Failed
  deriving (Show, Eq, Ord, Generic)

instance ToJSON ReportingStatus
instance FromJSON ReportingStatus

-- | Compliance record
data ComplianceRecord = ComplianceRecord
  { complianceRecordId :: UUID
  , complianceRecordTransactionId :: UUID
  , complianceRecordVerifications :: [CustomerVerification]
  , complianceRecordIsCompliant :: Bool
  , complianceRecordRequiresStateReporting :: Bool
  , complianceRecordReportingStatus :: ReportingStatus
  , complianceRecordReportedAt :: Maybe UTCTime
  , complianceRecordReferenceId :: Maybe Text
  , complianceRecordNotes :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ComplianceRecord
instance FromJSON ComplianceRecord

-- | Inventory status enumeration
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
  deriving (Show, Eq, Ord, Generic)

instance ToJSON InventoryStatus
instance FromJSON InventoryStatus