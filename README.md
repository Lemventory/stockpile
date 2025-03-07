# Cheeblr: Cannabis Dispensary Management System

A comprehensive full-stack web application for cannabis dispensary inventory, point-of-sale, and transaction management, utilizing PureScript for frontend development with Haskell backend services, all underpinned by a PostgreSQL database infrastructure.

![License](https://img.shields.io/badge/license-MIT-blue.svg)

## 📚 Documentation

Detailed documentation for each component of the system:

- [Nix Development Environment](./Docs/NixDevEnvironment.md) - Setup and configuration of the Nix-based development environment
- [Backend Documentation](./Docs/BackEnd.md) - Haskell backend API and database implementation
- [Frontend Documentation](./Docs/FrontEnd.md) - PureScript frontend application
- [Dependencies](./Docs/Dependencies.md) - List of dependencies
- [To Do list](./Docs/TODO.md) - List of future features and optimizations
- [Security Recommendations](./Docs/SecurityStrategies.md) - Detailed upgrades planned for security and authentication.

## 🌟 Features

### Inventory Management
- **Comprehensive Product Tracking**: Maintain detailed cannabis product information including strain data, THC/CBD content, terpenes, and lineage
- **Visual Categorization**: Products are visually distinguished by category and species
- **Flexible Sorting & Filtering**: Organize inventory by various criteria including name, category, quantity, and strain type
- **Complete CRUD Operations**: Support for creating, reading, updating, and deleting inventory items

### Point-of-Sale System
- **Transaction Processing**: Complete POS workflow for creating and finalizing sales transactions
- **Multiple Payment Methods**: Support for cash, credit, debit, ACH, and gift card payments
- **Tax Management**: Automatic calculation of sales and cannabis-specific taxes
- **Discount Application**: Apply percentage-based, fixed amount, or BOGO discounts
- **Receipt Generation**: Create formatted transaction receipts

### Financial Operations
- **Cash Register Management**: Open/close registers with cash tracking
- **Drawer Reconciliation**: End-of-shift reconciliation with variance tracking
- **Transaction Modifications**: Support for void and refund operations
- **Financial Reporting**: Generate daily sales and transaction reports

### Compliance Features
- **Customer Verification**: Age and medical card verification tracking
- **Purchase Limit Enforcement**: Monitor and enforce regulatory purchase limits
- **State Reporting**: Generate compliance reports for regulatory requirements
- **Product Labeling**: Generate compliant product labels with required information

## 🔧 Technology Stack

### Frontend
- **PureScript**: Strongly-typed functional programming language that compiles to JavaScript
- **Deku**: Declarative UI library for PureScript with hooks-like functionality
- **FRP**: Functional Reactive Programming for state management through Poll mechanism
- **Discrete Money Handling**: Precise currency operations with proper decimal handling

### Backend
- **Haskell**: Pure functional programming language for robust backend services
- **Servant**: Type-level web API library for defining type-safe endpoints
- **PostgreSQL**: Advanced open-source relational database with transaction support
- **Resource Pooling**: Efficient database connection management

### Development Environment
- **Nix**: Reproducible development environment with all dependencies
- **Cabal**: Haskell build system
- **Spago**: PureScript package manager and build tool
- **PostgreSQL Service**: Integrated database service through NixOS

## 🚀 Getting Started

### Prerequisites

- [Nix package manager](https://nixos.org/download.html) with flakes enabled

### Development Setup

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd cheeblr
   nix develop
   deploy
   ```

This will launch the entire setup (PostgreSQL NixOS systemd service and all).

### API Endpoints

The system exposes several API endpoints for different functional areas:

#### Inventory Endpoints
- `GET /inventory` - Retrieve all inventory items
- `POST /inventory` - Add a new inventory item
- `PUT /inventory` - Update an existing inventory item
- `DELETE /inventory/:sku` - Delete an inventory item

#### Transaction Endpoints
- `GET /transaction` - Get all transactions
- `POST /transaction` - Create a new transaction
- `POST /transaction/finalize/:id` - Finalize a transaction
- `POST /transaction/void/:id` - Void a transaction
- `POST /transaction/refund/:id` - Process a refund

#### Register Management
- `POST /register/open/:id` - Open a cash register
- `POST /register/close/:id` - Close a cash register

## 🔄 Current Development Status

The system currently has:

- A complete inventory management interface
- Full transaction processing capabilities
- Point-of-sale interface with payment processing
- Register opening/closing functionality
- Backend API for inventory and transaction operations
- PostgreSQL database integration with transaction support
- Type-safe communication between frontend and backend

## 🔍 Architecture

The application follows a layered architecture:

### Frontend Layers
1. **UI Components**: Deku-based components for different views
2. **State Management**: FRP mechanisms for reactive state handling
3. **Service Layer**: Business logic services for transaction processing, compliance, etc.
4. **API Integration**: Type-safe API clients for backend communication

### Backend Layers
1. **API Layer**: Servant-based API definitions
2. **Server Layer**: Request handling and business logic
3. **Database Layer**: PostgreSQL interaction and data persistence
4. **Domain Model**: Shared domain types between layers

## 📊 Data Models

The system manages several core data types:

- **MenuItem**: Product information with category, price, and quantity
- **StrainLineage**: Cannabis strain details including cannabinoid content and terpenes
- **Transaction**: Complete transaction record with items, payments, and status
- **Register**: Cash register state including drawer amounts and status

## 📜 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.