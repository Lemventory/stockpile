CREATE TABLE transaction_status (
  id TEXT PRIMARY KEY,
  description TEXT NOT NULL
);

INSERT INTO transaction_status (id, description) VALUES
  ('CREATED', 'Transaction has been created but not processed'),
  ('IN_PROGRESS', 'Transaction is being processed'),
  ('COMPLETED', 'Transaction has been successfully completed'),
  ('VOIDED', 'Transaction has been voided'),
  ('REFUNDED', 'Transaction has been refunded');

CREATE TABLE transaction_type (
  id TEXT PRIMARY KEY,
  description TEXT NOT NULL
);

INSERT INTO transaction_type (id, description) VALUES
  ('SALE', 'Standard sale transaction'),
  ('RETURN', 'Return transaction'),
  ('EXCHANGE', 'Exchange transaction'),
  ('INVENTORY_ADJUSTMENT', 'Inventory adjustment'),
  ('MANAGER_COMP', 'Manager comp transaction'),
  ('ADMINISTRATIVE', 'Administrative transaction');

CREATE TABLE payment_method (
  id TEXT PRIMARY KEY,
  description TEXT NOT NULL
);

INSERT INTO payment_method (id, description) VALUES
  ('CASH', 'Cash payment'),
  ('DEBIT', 'Debit card payment'),
  ('CREDIT', 'Credit card payment'),
  ('ACH', 'ACH transfer'),
  ('GIFT_CARD', 'Gift card payment'),
  ('STORED_VALUE', 'Stored value payment'),
  ('MIXED', 'Mixed payment methods'),
  ('OTHER', 'Other payment method');

CREATE TABLE tax_category (
  id TEXT PRIMARY KEY,
  description TEXT NOT NULL
);

INSERT INTO tax_category (id, description) VALUES
  ('REGULAR_SALES_TAX', 'Regular sales tax'),
  ('EXCISE_TAX', 'Excise tax'),
  ('CANNABIS_TAX', 'Cannabis-specific tax'),
  ('LOCAL_TAX', 'Local tax'),
  ('MEDICAL_TAX', 'Medical cannabis tax'),
  ('NO_TAX', 'No tax applies');

CREATE TABLE customer (
  id UUID PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT,
  phone TEXT,
  id_document TEXT,
  is_medical BOOLEAN NOT NULL DEFAULT FALSE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE TABLE employee (
  id UUID PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT NOT NULL,
  role TEXT NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE TABLE location (
  id UUID PRIMARY KEY,
  name TEXT NOT NULL,
  address TEXT NOT NULL,
  city TEXT NOT NULL,
  state TEXT NOT NULL,
  zip TEXT NOT NULL,
  phone TEXT,
  email TEXT,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE TABLE register (
  id UUID PRIMARY KEY,
  name TEXT NOT NULL,
  location_id UUID NOT NULL REFERENCES location(id) ON DELETE CASCADE,
  is_open BOOLEAN NOT NULL DEFAULT FALSE,
  current_drawer_amount DECIMAL(10,2) NOT NULL DEFAULT 0.00,
  expected_drawer_amount DECIMAL(10,2) NOT NULL DEFAULT 0.00,
  opened_at TIMESTAMP WITH TIME ZONE,
  opened_by UUID REFERENCES employee(id),
  last_transaction_time TIMESTAMP WITH TIME ZONE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE TABLE transaction (
  id UUID PRIMARY KEY,
  status TEXT NOT NULL REFERENCES transaction_status(id),
  created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  completed TIMESTAMP WITH TIME ZONE,
  customer_id UUID REFERENCES customer(id),
  employee_id UUID NOT NULL REFERENCES employee(id),
  register_id UUID NOT NULL REFERENCES register(id),
  location_id UUID NOT NULL REFERENCES location(id),
  subtotal DECIMAL(10,2) NOT NULL,
  discount_total DECIMAL(10,2) NOT NULL DEFAULT 0.00,
  tax_total DECIMAL(10,2) NOT NULL DEFAULT 0.00,
  total DECIMAL(10,2) NOT NULL,
  transaction_type TEXT NOT NULL REFERENCES transaction_type(id),
  is_voided BOOLEAN NOT NULL DEFAULT FALSE,
  void_reason TEXT,
  is_refunded BOOLEAN NOT NULL DEFAULT FALSE,
  refund_reason TEXT,
  reference_transaction_id UUID REFERENCES transaction(id),
  notes TEXT
);

CREATE TABLE transaction_item (
  id UUID PRIMARY KEY,
  transaction_id UUID NOT NULL REFERENCES transaction(id) ON DELETE CASCADE,
  menu_item_sku UUID NOT NULL REFERENCES menu_items(sku),
  quantity DECIMAL(10,3) NOT NULL,
  price_per_unit DECIMAL(10,2) NOT NULL,
  subtotal DECIMAL(10,2) NOT NULL,
  total DECIMAL(10,2) NOT NULL
);

CREATE TABLE discount_type (
  id TEXT PRIMARY KEY,
  description TEXT NOT NULL
);

INSERT INTO discount_type (id, description) VALUES
  ('PERCENT_OFF', 'Percentage off discount'),
  ('AMOUNT_OFF', 'Fixed amount off discount'),
  ('BUY_ONE_GET_ONE', 'Buy one get one free discount'),
  ('CUSTOM', 'Custom discount');

CREATE TABLE discount (
  id UUID PRIMARY KEY,
  transaction_id UUID REFERENCES transaction(id) ON DELETE CASCADE,
  transaction_item_id UUID REFERENCES transaction_item(id) ON DELETE CASCADE,
  type TEXT NOT NULL REFERENCES discount_type(id),
  amount DECIMAL(10,2) NOT NULL,
  percent DECIMAL(5,2),
  reason TEXT,
  approved_by UUID REFERENCES employee(id),
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  CONSTRAINT discount_transaction_check CHECK (
    (transaction_id IS NOT NULL AND transaction_item_id IS NULL) OR
    (transaction_id IS NULL AND transaction_item_id IS NOT NULL)
  )
);

CREATE TABLE transaction_tax (
  id UUID PRIMARY KEY,
  transaction_item_id UUID NOT NULL REFERENCES transaction_item(id) ON DELETE CASCADE,
  category TEXT NOT NULL REFERENCES tax_category(id),
  rate DECIMAL(5,2) NOT NULL,
  amount DECIMAL(10,2) NOT NULL,
  description TEXT
);

CREATE TABLE payment_transaction (
  id UUID PRIMARY KEY,
  transaction_id UUID NOT NULL REFERENCES transaction(id) ON DELETE CASCADE,
  method TEXT NOT NULL REFERENCES payment_method(id),
  amount DECIMAL(10,2) NOT NULL,
  tendered DECIMAL(10,2) NOT NULL,
  change_amount DECIMAL(10,2) NOT NULL DEFAULT 0.00,
  reference TEXT,
  approved BOOLEAN NOT NULL DEFAULT TRUE,
  authorization_code TEXT,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

-- Accounting-related tables

CREATE TABLE account_type (
  id TEXT PRIMARY KEY,
  description TEXT NOT NULL
);

INSERT INTO account_type (id, description) VALUES
  ('ASSET', 'Asset accounts'),
  ('LIABILITY', 'Liability accounts'),
  ('EQUITY', 'Equity accounts'),
  ('REVENUE', 'Revenue accounts'),
  ('EXPENSE', 'Expense accounts');

CREATE TABLE account (
  id UUID PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,
  name TEXT NOT NULL,
  is_debit_normal BOOLEAN NOT NULL,
  parent_account_id UUID REFERENCES account(id),
  account_type TEXT NOT NULL REFERENCES account_type(id),
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

-- Create standard accounts
INSERT INTO account (id, code, name, is_debit_normal, parent_account_id, account_type) VALUES
  ('00000000-0000-0000-0000-000000000001', '1000', 'Cash', TRUE, NULL, 'ASSET'),
  ('00000000-0000-0000-0000-000000000002', '1200', 'Accounts Receivable', TRUE, NULL, 'ASSET'),
  ('00000000-0000-0000-0000-000000000003', '1300', 'Inventory', TRUE, NULL, 'ASSET'),
  ('00000000-0000-0000-0000-000000000004', '2000', 'Accounts Payable', FALSE, NULL, 'LIABILITY'),
  ('00000000-0000-0000-0000-000000000005', '2100', 'Sales Tax Payable', FALSE, NULL, 'LIABILITY'),
  ('00000000-0000-0000-0000-000000000006', '2200', 'Cannabis Tax Payable', FALSE, NULL, 'LIABILITY'),
  ('00000000-0000-0000-0000-000000000007', '3000', 'Owner Equity', FALSE, NULL, 'EQUITY'),
  ('00000000-0000-0000-0000-000000000008', '4000', 'Sales Revenue', FALSE, NULL, 'REVENUE'),
  ('00000000-0000-0000-0000-000000000009', '5000', 'Cost of Goods Sold', TRUE, NULL, 'EXPENSE'),
  ('00000000-0000-0000-0000-000000000010', '5100', 'Inventory Adjustments', TRUE, NULL, 'EXPENSE'),
  ('00000000-0000-0000-0000-000000000011', '5200', 'Cash Over/Short', TRUE, NULL, 'EXPENSE');

CREATE TABLE ledger_entry_type (
  id TEXT PRIMARY KEY,
  description TEXT NOT NULL
);

INSERT INTO ledger_entry_type (id, description) VALUES
  ('SALE', 'Sale entry'),
  ('TAX', 'Tax entry'),
  ('DISCOUNT', 'Discount entry'),
  ('PAYMENT', 'Payment entry'),
  ('REFUND', 'Refund entry'),
  ('VOID', 'Void entry'),
  ('ADJUSTMENT', 'Adjustment entry'),
  ('FEE', 'Fee entry');

CREATE TABLE ledger_entry (
  id UUID PRIMARY KEY,
  transaction_id UUID NOT NULL REFERENCES transaction(id) ON DELETE CASCADE,
  account_id UUID NOT NULL REFERENCES account(id),
  amount DECIMAL(10,2) NOT NULL,
  is_debit BOOLEAN NOT NULL,
  entry_time TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  entry_type TEXT NOT NULL REFERENCES ledger_entry_type(id),
  description TEXT,
  created_by UUID REFERENCES employee(id),
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

-- Compliance-related tables

CREATE TABLE verification_type (
  id TEXT PRIMARY KEY,
  description TEXT NOT NULL
);

INSERT INTO verification_type (id, description) VALUES
  ('AGE_VERIFICATION', 'Age verification'),
  ('MEDICAL_CARD_VERIFICATION', 'Medical card verification'),
  ('ID_SCAN', 'ID scan verification'),
  ('VISUAL_INSPECTION', 'Visual inspection'),
  ('PATIENT_REGISTRATION', 'Patient registration'),
  ('PURCHASE_LIMIT_CHECK', 'Purchase limit check');

CREATE TABLE verification_status (
  id TEXT PRIMARY KEY,
  description TEXT NOT NULL
);

INSERT INTO verification_status (id, description) VALUES
  ('VERIFIED', 'Verification successful'),
  ('FAILED', 'Verification failed'),
  ('EXPIRED', 'Verification expired'),
  ('NOT_REQUIRED', 'Verification not required');

CREATE TABLE customer_verification (
  id UUID PRIMARY KEY,
  customer_id UUID NOT NULL REFERENCES customer(id) ON DELETE CASCADE,
  verification_type TEXT NOT NULL REFERENCES verification_type(id),
  status TEXT NOT NULL REFERENCES verification_status(id),
  verified_by UUID NOT NULL REFERENCES employee(id),
  verified_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  expires_at TIMESTAMP WITH TIME ZONE,
  notes TEXT,
  document_id TEXT
);

CREATE TABLE reporting_status (
  id TEXT PRIMARY KEY,
  description TEXT NOT NULL
);

INSERT INTO reporting_status (id, description) VALUES
  ('NOT_REQUIRED', 'State reporting not required'),
  ('PENDING', 'Pending submission to state'),
  ('SUBMITTED', 'Submitted to state'),
  ('ACKNOWLEDGED', 'Acknowledged by state'),
  ('FAILED', 'Failed state submission');

CREATE TABLE compliance_record (
  id UUID PRIMARY KEY,
  transaction_id UUID NOT NULL REFERENCES transaction(id) ON DELETE CASCADE,
  is_compliant BOOLEAN NOT NULL DEFAULT TRUE,
  requires_state_reporting BOOLEAN NOT NULL DEFAULT FALSE,
  reporting_status TEXT NOT NULL REFERENCES reporting_status(id),
  reported_at TIMESTAMP WITH TIME ZONE,
  reference_id TEXT,
  notes TEXT,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE TABLE compliance_verification (
  compliance_record_id UUID NOT NULL REFERENCES compliance_record(id) ON DELETE CASCADE,
  customer_verification_id UUID NOT NULL REFERENCES customer_verification(id),
  PRIMARY KEY (compliance_record_id, customer_verification_id)
);

-- Inventory tracking for compliance

CREATE TABLE inventory_status (
  id TEXT PRIMARY KEY,
  description TEXT NOT NULL
);

INSERT INTO inventory_status (id, description) VALUES
  ('AVAILABLE', 'Available for sale'),
  ('ON_HOLD', 'On hold'),
  ('RESERVED', 'Reserved for customer'),
  ('SOLD', 'Sold'),
  ('DAMAGED', 'Damaged'),
  ('EXPIRED', 'Expired'),
  ('IN_TRANSIT', 'In transit'),
  ('UNDER_REVIEW', 'Under review'),
  ('RECALLED', 'Recalled');

-- Add indexes for better performance
CREATE INDEX transaction_employee_idx ON transaction(employee_id);
CREATE INDEX transaction_register_idx ON transaction(register_id);
CREATE INDEX transaction_location_idx ON transaction(location_id);
CREATE INDEX transaction_customer_idx ON transaction(customer_id);
CREATE INDEX transaction_reference_idx ON transaction(reference_transaction_id);
CREATE INDEX transaction_item_transaction_idx ON transaction_item(transaction_id);
CREATE INDEX transaction_item_menu_item_idx ON transaction_item(menu_item_sku);
CREATE INDEX payment_transaction_transaction_idx ON payment_transaction(transaction_id);
CREATE INDEX ledger_entry_transaction_idx ON ledger_entry(transaction_id);
CREATE INDEX ledger_entry_account_idx ON ledger_entry(account_id);

-- Function to automatically update updated_at timestamps
CREATE OR REPLACE FUNCTION update_modified_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create triggers for updated_at columns
CREATE TRIGGER update_customer_modtime
    BEFORE UPDATE ON customer
    FOR EACH ROW
    EXECUTE FUNCTION update_modified_column();

CREATE TRIGGER update_employee_modtime
    BEFORE UPDATE ON employee
    FOR EACH ROW
    EXECUTE FUNCTION update_modified_column();

CREATE TRIGGER update_location_modtime
    BEFORE UPDATE ON location
    FOR EACH ROW
    EXECUTE FUNCTION update_modified_column();

CREATE TRIGGER update_register_modtime
    BEFORE UPDATE ON register
    FOR EACH ROW
    EXECUTE FUNCTION update_modified_column();

CREATE TRIGGER update_account_modtime
    BEFORE UPDATE ON account
    FOR EACH ROW
    EXECUTE FUNCTION update_modified_column();

CREATE TRIGGER update_compliance_record_modtime
    BEFORE UPDATE ON compliance_record
    FOR EACH ROW
    EXECUTE FUNCTION update_modified_column();