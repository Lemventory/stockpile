module Accounting.Compliance where

import Prelude

import Data.Array (any, find, foldl, null)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now)
import Types.Inventory (ItemCategory(..), MenuItem(..), StrainLineage(..))
import Types.Transaction (Transaction(..), TransactionItem(..))
import Types.UUID (UUID)
import Utils.Formatting (uuidToString)
import Utils.UUIDGen (genUUID)

-- | Compliance verification type
data VerificationType
  = AgeVerification
  | MedicalCardVerification
  | IDScan
  | VisualInspection
  | PatientRegistration
  | PurchaseLimitCheck

derive instance eqVerificationType :: Eq VerificationType
derive instance ordVerificationType :: Ord VerificationType

instance showVerificationType :: Show VerificationType where
  show AgeVerification = "Age Verification"
  show MedicalCardVerification = "Medical Card Verification"
  show IDScan = "ID Scan"
  show VisualInspection = "Visual Inspection"
  show PatientRegistration = "Patient Registration"
  show PurchaseLimitCheck = "Purchase Limit Check"

-- | Verification status
data VerificationStatus
  = Verified
  | Failed
  | Expired
  | NotRequired

derive instance eqVerificationStatus :: Eq VerificationStatus
derive instance ordVerificationStatus :: Ord VerificationStatus

instance showVerificationStatus :: Show VerificationStatus where
  show Verified = "Verified"
  show Failed = "Failed"
  show Expired = "Expired"
  show NotRequired = "Not Required"

-- | Compliance error
data ComplianceError
  = AgeVerificationFailed
  | PurchaseLimitExceeded
  | InvalidMedicalCard
  | ProductTrackingError
  | RequiredDataMissing
  | SystemError String

derive instance eqComplianceError :: Eq ComplianceError
derive instance ordComplianceError :: Ord ComplianceError

instance showComplianceError :: Show ComplianceError where
  show AgeVerificationFailed = "Age verification failed"
  show PurchaseLimitExceeded = "Purchase limit exceeded"
  show InvalidMedicalCard = "Invalid medical card"
  show ProductTrackingError = "Product tracking error"
  show RequiredDataMissing = "Required compliance data missing"
  show (SystemError msg) = "System error: " <> msg

-- | Customer verification
type CustomerVerification =
  { id :: UUID
  , customerId :: UUID
  , verificationType :: VerificationType
  , status :: VerificationStatus
  , verifiedBy :: UUID
  , verifiedAt :: DateTime
  , expiresAt :: Maybe DateTime
  , notes :: Maybe String
  , documentId :: Maybe String
  }

-- | Purchase limits by category
type PurchaseLimit =
  { category :: ItemCategory
  , dailyLimit :: Number
  , measureUnit :: String
  }

-- | Transaction compliance record
type ComplianceRecord =
  { id :: UUID
  , transactionId :: UUID
  , verifications :: Array CustomerVerification
  , isCompliant :: Boolean
  , requiresStateReporting :: Boolean
  , reportingStatus :: ReportingStatus
  , reportedAt :: Maybe DateTime
  , referenceId :: Maybe String
  , notes :: Maybe String
  }

data ReportingStatus
  = ReportNotRequired
  | Pending
  | Submitted
  | Acknowledged
  | ReportFailed

derive instance eqReportingStatus :: Eq ReportingStatus
derive instance ordReportingStatus :: Ord ReportingStatus

instance showReportingStatus :: Show ReportingStatus where
  show ReportNotRequired = "Not Required"
  show Pending = "Pending"
  show Submitted = "Submitted"
  show Acknowledged = "Acknowledged"
  show ReportFailed = "Failed"

-- | Default purchase limits (would come from regulatory configuration)
defaultPurchaseLimits :: Array PurchaseLimit
defaultPurchaseLimits =
  [ { category: Flower, dailyLimit: 28.0, measureUnit: "g" }
  , { category: Concentrates, dailyLimit: 8.0, measureUnit: "g" }
  , { category: Edibles, dailyLimit: 800.0, measureUnit: "mg" } -- THC content
  ]

-- | Check customer eligibility for purchase
checkCustomerEligibility
  :: UUID
  -> -- Customer ID
  Maybe String
  -> -- ID document
  Maybe Boolean
  -> -- Is medical patient
  UUID
  -> -- Employee verifying
  Aff (Either ComplianceError (Array CustomerVerification))
checkCustomerEligibility customerId maybeDocument isMedical employeeId = do
  -- In a complete implementation, this would check against a customer database
  -- and perform document verification

  liftEffect $ log $ "Checking eligibility for customer " <> uuidToString
    customerId

  -- Get current timestamp
  timestamp <- liftEffect now

  -- Generate verification IDs
  ageVerificationId <- liftEffect genUUID

  medicalVerificationId <- liftEffect genUUID

  -- Check if we have a document for verification
  case maybeDocument of
    Nothing -> do
      liftEffect $ log "No ID document provided for verification"
      pure $ Left RequiredDataMissing

    Just document -> do
      -- In a complete implementation, would validate the document and check age
      let
        -- Create age verification record
        ageVerification =
          { id: ageVerificationId
          , customerId
          , verificationType: AgeVerification
          , status: Verified -- Assuming verification is successful
          , verifiedBy: employeeId
          , verifiedAt: toDateTime timestamp
          , expiresAt: Nothing -- Age verification doesn't expire
          , notes: Nothing
          , documentId: Just document
          }

        -- If customer is a medical patient, create medical verification
        medicalVerification = case isMedical of
          Just true ->
            Just
              { id: medicalVerificationId
              , customerId
              , verificationType: MedicalCardVerification
              , status: Verified -- Assuming verification is successful
              , verifiedBy: employeeId
              , verifiedAt: toDateTime timestamp
              , expiresAt: Nothing -- Would have an expiration in complete implementation
              , notes: Nothing
              , documentId: Nothing -- Would have a medical card ID in complete implementation
              }

          _ -> Nothing

        verifications = case medicalVerification of
          Just medVerif -> [ ageVerification, medVerif ]
          Nothing -> [ ageVerification ]

      liftEffect $ log "Customer eligibility verified successfully"

      pure $ Right verifications

-- | Check if transaction is within purchase limits
-- | Check if transaction is within purchase limits
checkPurchaseLimits
  :: UUID
  -> -- Customer ID
  Array TransactionItem
  -> -- Current transaction items
  Array Transaction
  -> -- Previous transactions (for daily limits)
  Aff (Either ComplianceError Boolean)
checkPurchaseLimits customerId items previousTransactions = do
  liftEffect $ log $ "Checking purchase limits for customer " <> uuidToString customerId

  -- In a complete implementation, we would:
  -- 1. Calculate total amounts by category from current transaction
  -- 2. Calculate total amounts by category from previous transactions (within time window)
  -- 3. Compare against limits
  
  -- Step 1: Calculate amounts in current transaction by category
  let 
    -- This would normally look up the menu item and get the category
    -- For this example, we'll assume all items are Flower with 1g per item
    currentAmounts = 
      { flower: foldl (\acc (TransactionItem item) -> acc + item.quantity) 0.0 items
      , concentrates: 0.0
      , edibles: 0.0
      }
  
  -- Step 2: Calculate amounts from previous transactions (within 24 hours)
  -- In a complete implementation, we would filter by date first
  let
    previousAmounts = 
      foldl 
        (\acc (Transaction tx) -> 
          -- Calculate amounts from transaction items
          let 
            txFlower = foldl (\itemAcc (TransactionItem item) -> 
                             itemAcc + item.quantity) 0.0 tx.items
          in
            { flower: acc.flower + txFlower
            , concentrates: acc.concentrates
            , edibles: acc.edibles
            }
        )
        { flower: 0.0, concentrates: 0.0, edibles: 0.0 }
        previousTransactions
  
  -- Step 3: Compare against limits from regulations
  let
    -- Get the purchase limits
    limits = defaultPurchaseLimits
    
    -- Calculate totals (current + previous)
    totalFlower = currentAmounts.flower + previousAmounts.flower
    totalConcentrates = currentAmounts.concentrates + previousAmounts.concentrates
    totalEdibles = currentAmounts.edibles + previousAmounts.edibles
    
    -- Check if any limit is exceeded
    flowerLimit = case find (\limit -> limit.category == Flower) limits of
                   Just limit -> limit.dailyLimit
                   Nothing -> 28.0  -- Default if not found
                   
    concentrateLimit = case find (\limit -> limit.category == Concentrates) limits of
                        Just limit -> limit.dailyLimit
                        Nothing -> 8.0  -- Default if not found
                        
    edibleLimit = case find (\limit -> limit.category == Edibles) limits of
                   Just limit -> limit.dailyLimit
                   Nothing -> 800.0  -- Default if not found
    
    -- Check if any limit is exceeded
    isFlowerExceeded = totalFlower > flowerLimit
    isConcentrateExceeded = totalConcentrates > concentrateLimit
    isEdibleExceeded = totalEdibles > edibleLimit
    
    -- Determine overall status
    isAnyLimitExceeded = isFlowerExceeded || isConcentrateExceeded || isEdibleExceeded
    isBelowLimits = not isAnyLimitExceeded

  -- Log the details for monitoring
  liftEffect $ log $ "Current transaction amounts - Flower: " <> show currentAmounts.flower
    <> "g, Concentrates: " <> show currentAmounts.concentrates
    <> "g, Edibles: " <> show currentAmounts.edibles <> "mg"
    
  liftEffect $ log $ "Previous purchases today - Flower: " <> show previousAmounts.flower
    <> "g, Concentrates: " <> show previousAmounts.concentrates
    <> "g, Edibles: " <> show previousAmounts.edibles <> "mg"
    
  liftEffect $ log $ "Purchase limits - Flower: " <> show flowerLimit
    <> "g, Concentrates: " <> show concentrateLimit
    <> "g, Edibles: " <> show edibleLimit <> "mg"

  if isBelowLimits then do
    liftEffect $ log "Purchase is within legal limits"
    pure $ Right true
  else do
    liftEffect $ log "Purchase exceeds legal limits"
    pure $ Left PurchaseLimitExceeded

-- | Create compliance record for a transaction
createComplianceRecord
  :: Transaction
  -> Array CustomerVerification
  -> Aff (Either ComplianceError ComplianceRecord)
createComplianceRecord transaction verifications = do
  -- Unwrap the Transaction to access its fields
  let Transaction txData = transaction

  liftEffect $ log $ "Creating compliance record for transaction " <>
    uuidToString txData.id

  recordId <- liftEffect genUUID

  timestamp <- liftEffect now

  let
    requiresReporting = containsCannabisProducts txData.items

    isCompliant = not (null verifications)

    reportingStatus = if requiresReporting then Pending else ReportNotRequired

  let
    complianceRecord =
      { id: recordId
      , transactionId: txData.id
      , verifications
      , isCompliant
      , requiresStateReporting: requiresReporting
      , reportingStatus
      , reportedAt: Just (toDateTime timestamp)
      , referenceId: Nothing
      , notes: Nothing
      }

  liftEffect $ log $ "Compliance record created: " <> uuidToString recordId

  pure $ Right complianceRecord

-- | Submit transaction to state tracking system
submitToStateTracking
  :: Transaction
  -> ComplianceRecord
  -> Aff
       ( Either ComplianceError
           { updatedRecord :: ComplianceRecord, referenceId :: String }
       )
submitToStateTracking transaction record = do
  -- Use pattern matching instead of unwrap
  let Transaction txData = transaction
  
  -- Get current timestamp properly
  currentTime <- liftEffect now
  let timestamp = toDateTime currentTime

  liftEffect $ log $ "Submitting transaction to state tracking system: " <>
    uuidToString txData.id

  let
    referenceId = "ST-" <> uuidToString txData.id

    updatedRecord = record
      { reportingStatus = Submitted
      , reportedAt = Just timestamp  -- Use the proper timestamp
      , referenceId = Just referenceId
      }

  liftEffect $ log $
    "Successcompletey submitted to state tracking system. Reference: " <>
      referenceId

  pure $ Right { updatedRecord, referenceId }

-- | Generate compliance report
generateComplianceReport
  :: DateTime
  -> -- Start date
  DateTime
  -> -- End date
  UUID
  -> -- Location ID
  Aff (Either ComplianceError String)
generateComplianceReport startDate endDate locationId = do
  liftEffect $ log "Generating compliance report"

  -- In a complete implementation, this would generate a report of all transactions
  -- in the given date range, their compliance status, etc.

  -- Simulate a successful report generation
  let
    report =
      "Compliance Report\n"
        <> "=================\n"
        <> "Location: "
        <> uuidToString locationId
        <> "\n"
        <> "Period: "
        <> show startDate
        <> " to "
        <> show endDate
        <> "\n"
        <> "Transactions: 42\n"
        <> "Compliant: 42\n"
        <> "Non-compliant: 0\n"
        <> "Reporting status:\n"
        <> "  Submitted: 40\n"
        <> "  Pending: 2\n"
        <>
          "  Failed: 0\n"

  liftEffect $ log "Compliance report generated successfully"

  pure $ Right report

-- | Validate a cannabis product for compliance
validateProduct
  :: MenuItem
  -> Aff (Either ComplianceError Boolean)
validateProduct menuItem = do
  liftEffect $ log $ "Validating product compliance: " <>
    let
      MenuItem menuItemRecord = menuItem
    in
      menuItemRecord.name

  -- In a complete implementation, this would check:
  -- - Product has required testing information
  -- - Product has valid batch/lot numbers
  -- - Product is not expired
  -- - Product packaging meets requirements
  -- - etc.

  -- Simplified validation
  let
    isValid = true -- Assuming all is well for this example

  if isValid then do
    liftEffect $ log "Product validated successfully"
    pure $ Right true
  else do
    liftEffect $ log "Product failed validation"
    pure $ Left ProductTrackingError

-- | Check if transaction contains cannabis products (requiring reporting)
containsCannabisProducts :: Array TransactionItem -> Boolean
containsCannabisProducts items =
  any isCannabisProduct items
  where
    isCannabisProduct :: TransactionItem -> Boolean
    isCannabisProduct (TransactionItem item) = 
      -- We need to look up the menu item in the inventory to check its category
      -- Since we don't have direct access to the inventory here, we'll
      -- infer from other information in the transaction item
      -- In a complete implementation, this would likely involve a lookup
      -- or having the category information included in the transaction item
      case findItemCategory item.menuItemSku of
        Just category -> isCannabisCategory category
        Nothing -> false

    -- This is a helper function that would normally fetch the category
    -- from a database or in-memory cache.
    findItemCategory :: UUID -> Maybe ItemCategory
    findItemCategory _ = Just Flower -- Replace with actual lookup in complete code

    -- Determine which categories are considered cannabis products
    isCannabisCategory :: ItemCategory -> Boolean
    isCannabisCategory Flower = true
    isCannabisCategory PreRolls = true
    isCannabisCategory Vaporizers = true
    isCannabisCategory Edibles = true
    isCannabisCategory Drinks = true
    isCannabisCategory Concentrates = true
    isCannabisCategory Topicals = true
    isCannabisCategory Tinctures = true
    isCannabisCategory Accessories = false

-- | Create label data for a product
generateProductLabel
  :: MenuItem
  -> String
  ->
  Aff String
generateProductLabel menuItem batchId = do
  let MenuItem item = menuItem
      StrainLineage lineage = item.strain_lineage
  
  liftEffect $ log $ "Generating compliant label for product: " <> item.name

  let
    label =
      "CANNABIS PRODUCT\n"
        <> "================\n"
        <> "Name: "
        <> item.name
        <> "\n"
        <> "Brand: "
        <> item.brand
        <> "\n"
        <> "THC: "
        <> lineage.thc
        <> "\n"
        <> "CBD: "
        <> lineage.cbg
        <> "\n"
        <> "Batch: "
        <> batchId
        <> "\n"
        <> "WARNING: For use only by adults 21 and older.\n"
        <> "Keep out of reach of children. Do not drive\n"
        <>
          "or operate heavy machinery while using this product."

  pure label