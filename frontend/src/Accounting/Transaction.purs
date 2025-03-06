module Types.Transaction where

import Prelude

import Data.Maybe (Maybe)
import Data.UUID (UUID)
import Data.Finance.Currency (USD)
import Data.Finance.Money (Discrete)
import Data.DateTime (DateTime)
import Data.Newtype (class Newtype)

-- | Basic ID type for all entities
type ID :: forall k. k -> Type
type ID a = UUID

-- | Transaction Status
data TransactionStatus 
  = Created
  | InProgress
  | Completed
  | Voided
  | Refunded

derive instance eqTransactionStatus :: Eq TransactionStatus
derive instance ordTransactionStatus :: Ord TransactionStatus

instance showTransactionStatus :: Show TransactionStatus where
  show Created = "Created"
  show InProgress = "In Progress"
  show Completed = "Completed"
  show Voided = "Voided"
  show Refunded = "Refunded"

-- | Payment Method
data PaymentMethod
  = Cash
  | Debit
  | Credit
  | ACH
  | GiftCard
  | StoredValue
  | Mixed
  | Other String

derive instance eqPaymentMethod :: Eq PaymentMethod
derive instance ordPaymentMethod :: Ord PaymentMethod

instance showPaymentMethod :: Show PaymentMethod where
  show Cash = "Cash"
  show Debit = "Debit"
  show Credit = "Credit"
  show ACH = "ACH"
  show GiftCard = "Gift Card"
  show StoredValue = "Stored Value"
  show Mixed = "Mixed Payment"
  show (Other s) = "Other: " <> s

-- | Discount Type
data DiscountType
  = PercentOff Number  -- 0.10 for 10% off
  | AmountOff (Discrete USD)  -- Fixed amount off
  | BuyOneGetOne  -- BOGO
  | Custom String (Discrete USD)  -- Custom with equivalent value

derive instance eqDiscountType :: Eq DiscountType
derive instance ordDiscountType :: Ord DiscountType

-- | Tax Category
data TaxCategory
  = RegularSalesTax
  | ExciseTax
  | CannabisTax
  | LocalTax
  | MedicalTax
  | NoTax  -- Tax exempt

derive instance eqTaxCategory :: Eq TaxCategory
derive instance ordTaxCategory :: Ord TaxCategory

-- | Tax Record
type TaxRecord = 
  { category :: TaxCategory
  , rate :: Number
  , amount :: Discrete USD
  , description :: String
  }

-- | Discount Record
type DiscountRecord =
  { type :: DiscountType
  , amount :: Discrete USD
  , reason :: String
  , approvedBy :: Maybe UUID -- ID Employee
  }

-- | Transaction Item - using newtype to break the cycle
newtype TransactionItem = TransactionItem
  { id :: UUID
  , transactionId :: UUID -- ID Transaction
  , menuItemSku :: UUID -- ID MenuItem
  , quantity :: Number
  , pricePerUnit :: Discrete USD
  , discounts :: Array DiscountRecord
  , taxes :: Array TaxRecord
  , subtotal :: Discrete USD  -- Price before discounts and taxes
  , total :: Discrete USD     -- Final price after discounts and taxes
  }

derive instance newtypeTransactionItem :: Newtype TransactionItem _
derive instance eqTransactionItem :: Eq TransactionItem
derive instance ordTransactionItem :: Ord TransactionItem

-- | Payment Transaction - using newtype to break the cycle
newtype PaymentTransaction = PaymentTransaction
  { id :: UUID
  , transactionId :: UUID -- ID Transaction
  , method :: PaymentMethod
  , amount :: Discrete USD
  , tendered :: Discrete USD  -- Amount customer provided
  , change :: Discrete USD    -- Change returned to customer
  , reference :: Maybe String -- Reference number, last 4 of card, etc.
  , approved :: Boolean
  , authorizationCode :: Maybe String
  }

derive instance newtypePaymentTransaction :: Newtype PaymentTransaction _
derive instance eqPaymentTransaction :: Eq PaymentTransaction
derive instance ordPaymentTransaction :: Ord PaymentTransaction

-- | Transaction (Main type) - using newtype to break the cycle
newtype Transaction = Transaction
  { id :: UUID
  , status :: TransactionStatus
  , created :: DateTime
  , completed :: Maybe DateTime
  , customer :: Maybe UUID -- ID Customer
  , employee :: UUID -- ID Employee
  , register :: UUID -- ID Register
  , location :: UUID -- ID Location
  , items :: Array TransactionItem
  , payments :: Array PaymentTransaction
  , subtotal :: Discrete USD  -- Sum of all items before discounts and taxes
  , discountTotal :: Discrete USD  -- Sum of all discounts
  , taxTotal :: Discrete USD  -- Sum of all taxes
  , total :: Discrete USD  -- Final amount
  , transactionType :: TransactionType
  , isVoided :: Boolean
  , voidReason :: Maybe String
  , isRefunded :: Boolean
  , refundReason :: Maybe String
  , referenceTransactionId :: Maybe UUID -- ID Transaction
  , notes :: Maybe String
  }

derive instance newtypeTransaction :: Newtype Transaction _
derive instance eqTransaction :: Eq Transaction
derive instance ordTransaction :: Ord Transaction

-- | Transaction Type
data TransactionType
  = Sale
  | Return
  | Exchange
  | InventoryAdjustment
  | ManagerComp
  | Administrative

derive instance eqTransactionType :: Eq TransactionType
derive instance ordTransactionType :: Ord TransactionType

instance showTransactionType :: Show TransactionType where
  show Sale = "Sale"
  show Return = "Return"
  show Exchange = "Exchange"
  show InventoryAdjustment = "Inventory Adjustment"
  show ManagerComp = "Manager Comp"
  show Administrative = "Administrative"

-- | Ledger Entry - using newtype to break the cycle
newtype LedgerEntry = LedgerEntry
  { id :: UUID
  , transactionId :: UUID -- ID Transaction
  , accountId :: UUID -- ID Account
  , amount :: Discrete USD
  , isDebit :: Boolean
  , timestamp :: DateTime
  , entryType :: LedgerEntryType
  , description :: String
  }

derive instance newtypeLedgerEntry :: Newtype LedgerEntry _
derive instance eqLedgerEntry :: Eq LedgerEntry
derive instance ordLedgerEntry :: Ord LedgerEntry

-- | Ledger Entry Type
data LedgerEntryType
  = SaleEntry
  | Tax
  | Discount
  | Payment
  | Refund
  | Void
  | Adjustment
  | Fee

derive instance eqLedgerEntryType :: Eq LedgerEntryType
derive instance ordLedgerEntryType :: Ord LedgerEntryType

instance showLedgerEntryType :: Show LedgerEntryType where
  show SaleEntry = "Sale"
  show Tax = "Tax"
  show Discount = "Discount"
  show Payment = "Payment"
  show Refund = "Refund"
  show Void = "Void"
  show Adjustment = "Adjustment"
  show Fee = "Fee"

-- | Account Type
data AccountType
  = Asset
  | Liability
  | Equity
  | Revenue
  | Expense

derive instance eqAccountType :: Eq AccountType
derive instance ordAccountType :: Ord AccountType

instance showAccountType :: Show AccountType where
  show Asset = "Asset"
  show Liability = "Liability"
  show Equity = "Equity"
  show Revenue = "Revenue"
  show Expense = "Expense"

-- | Account - using newtype to break the cycle
newtype Account = Account
  { id :: UUID
  , code :: String
  , name :: String
  , isDebitNormal :: Boolean  -- True for asset/expense accounts, False for liability/revenue/equity
  , parentAccount :: Maybe UUID -- ID Account
  , accountType :: AccountType
  }

derive instance newtypeAccount :: Newtype Account _
derive instance eqAccount :: Eq Account
derive instance ordAccount :: Ord Account

-- | Item Category (based on existing backend)
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
  | Merchandise

derive instance eqItemCategory :: Eq ItemCategory
derive instance ordItemCategory :: Ord ItemCategory

instance showItemCategory :: Show ItemCategory where
  show Flower = "Flower"
  show PreRolls = "Pre-Rolls"
  show Vaporizers = "Vaporizers"
  show Edibles = "Edibles"
  show Drinks = "Drinks"
  show Concentrates = "Concentrates"
  show Topicals = "Topicals"
  show Tinctures = "Tinctures"
  show Accessories = "Accessories"
  show Merchandise = "Merchandise"

-- | Species (based on existing backend)
data Species
  = Indica
  | IndicaDominantHybrid
  | Hybrid
  | SativaDominantHybrid
  | Sativa

derive instance eqSpecies :: Eq Species
derive instance ordSpecies :: Ord Species

instance showSpecies :: Show Species where
  show Indica = "Indica"
  show IndicaDominantHybrid = "Indica-dominant Hybrid"
  show Hybrid = "Hybrid"
  show SativaDominantHybrid = "Sativa-dominant Hybrid"
  show Sativa = "Sativa"

-- | Strain Lineage (based on existing backend)
type StrainLineage =
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

-- | Menu Item (based on existing backend) - using newtype to break the cycle
newtype MenuItem = MenuItem
  { sort :: Int
  , sku :: UUID
  , brand :: String
  , name :: String
  , price :: Number
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

derive instance newtypeMenuItem :: Newtype MenuItem _
derive instance eqMenuItem :: Eq MenuItem
derive instance ordMenuItem :: Ord MenuItem

-- | Show instances for debugging
instance showTransaction :: Show Transaction where
  show (Transaction t) = 
    "Transaction { id: " <> show t.id <> 
    ", total: " <> show t.total <> 
    ", status: " <> show t.status <> " }"

instance showTransactionItem :: Show TransactionItem where
  show (TransactionItem ti) = 
    "TransactionItem { sku: " <> show ti.menuItemSku <> 
    ", quantity: " <> show ti.quantity <> 
    ", total: " <> show ti.total <> " }"

instance showPaymentTransaction :: Show PaymentTransaction where
  show (PaymentTransaction pt) = 
    "Payment { method: " <> show pt.method <> 
    ", amount: " <> show pt.amount <> " }"

instance showMenuItem :: Show MenuItem where
  show (MenuItem mi) = 
    "MenuItem { name: " <> mi.name <> 
    ", price: " <> show mi.price <> 
    ", category: " <> show mi.category <> " }"

instance showLedgerEntry :: Show LedgerEntry where
  show (LedgerEntry le) = 
    "LedgerEntry { entryType: " <> show le.entryType <> 
    ", amount: " <> show le.amount <> 
    ", isDebit: " <> show le.isDebit <> " }"

instance showAccount :: Show Account where
  show (Account a) = 
    "Account { name: " <> a.name <> 
    ", type: " <> show a.accountType <> " }"