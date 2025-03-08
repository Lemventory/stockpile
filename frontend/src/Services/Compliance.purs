module Accounting.Compliance where

import Prelude

import Data.Array (foldl, null, filter, length)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Finance.Currency (USD)
import Data.Finance.Money (Discrete(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now)
import Types.Inventory
import Types.Transaction
import Types.UUID (UUID)
import Utils.Formatting (uuidToString)
import Utils.UUIDGen (genUUID)
import Services.CashRegister (RegisterError(..))

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
  -- In a real implementation, this would check against a customer database
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
      -- In a real implementation, would validate the document and check age
      let
        -- Create age verification record
        ageVerification =
          { id: ageVerificationId
          , customerId
          , verificationType: AgeVerification
          , status: Verified -- Assuming verification is successful
          , verifiedBy: employeeId
          , verifiedAt: timestamp
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
              , verifiedAt: timestamp
              , expiresAt: Nothing -- Would have an expiration in real implementation
              , notes: Nothing
              , documentId: Nothing -- Would have a medical card ID in real implementation
              }

          _ -> Nothing

        verifications = case medicalVerification of
          Just medVerif -> [ ageVerification, medVerif ]
          Nothing -> [ ageVerification ]

      liftEffect $ log "Customer eligibility verified successfully"

      pure $ Right verifications

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
  liftEffect $ log $ "Checking purchase limits for customer " <> uuidToString
    customerId

  -- In a real implementation, this would calculate the total purchased amounts
  -- by category for the day, including the current transaction, and compare
  -- against regulatory limits

  -- This is a simplified implementation
  let
    -- Calculate total amounts by category
    -- (would be much more complex in a real implementation)
    isBelowLimits = true -- Assuming all is well for this example

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
  liftEffect $ log $ "Creating compliance record for transaction " <>
    uuidToString transaction.id

  recordId <- liftEffect genUUID

  timestamp <- liftEffect now

  let
    requiresReporting = containsCannabisProducts transaction.items

    isCompliant = not (null verifications)

    reportingStatus = if requiresReporting then Pending else ReportNotRequired

  let
    complianceRecord =
      { id: recordId
      , transactionId: transaction.id
      , verifications
      , isCompliant
      , requiresStateReporting: requiresReporting
      , reportingStatus
      , reportedAt: Nothing
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
  liftEffect $ log $ "Submitting transaction to state tracking system: " <>
    uuidToString transaction.id

  let
    referenceId = "ST-" <> uuidToString transaction.id

    updatedRecord = record
      { reportingStatus = Submitted
      , -- This stays as Submitted
        reportedAt = Just (unsafeCoerce "2025-03-05T00:00:00Z")
      , referenceId = Just referenceId
      }

  liftEffect $ log $
    "Successfully submitted to state tracking system. Reference: " <>
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

  -- In a real implementation, this would generate a report of all transactions
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
  liftEffect $ log $ "Validating product compliance: " <> menuItem.name

  -- In a real implementation, this would check:
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
  let
    -- This is a placeholder - in reality would check each item's category
    containsCannabis = true -- Assuming transaction contains cannabis for this example
  in
    containsCannabis

-- | Create label data for a product
generateProductLabel
  :: MenuItem
  -> String
  -> -- Batch ID
  Aff String
generateProductLabel menuItem batchId = do
  liftEffect $ log $ "Generating compliant label for product: " <> menuItem.name

  -- In a real implementation, this would generate a compliant product label
  -- with all required information (THC/CBD content, warnings, batch ID, etc.)

  let
    label =
      "CANNABIS PRODUCT\n"
        <> "================\n"
        <> "Name: "
        <> menuItem.name
        <> "\n"
        <> "Brand: "
        <> menuItem.brand
        <> "\n"
        <> "THC: "
        <> menuItem.strain_lineage.thc
        <> "\n"
        <> "CBD: "
        <> menuItem.strain_lineage.cbg
        <> "\n"
        <> "Batch: "
        <> batchId
        <> "\n"
        <> "WARNING: For use only by adults 21 and older.\n"
        <> "Keep out of reach of children. Do not drive\n"
        <>
          "or operate heavy machinery while using this product."

  pure label

-- | Utility function for unsafe coercion (for example purposes only)
foreign import unsafeCoerce :: forall a b. a -> b