# Cheeblr Backend Documentation

This document provides detailed information about the Haskell backend for the Cheeblr application.

## Table of Contents

- [Overview](#overview)
- [Technologies](#technologies)
- [Architecture](#architecture)
- [API Reference](#api-reference)
- [Database Schema](#database-schema)
- [Data Types](#data-types)
- [Deployment](#deployment)
- [Development Guidelines](#development-guidelines)

## Overview

The Cheeblr backend is a Haskell-based REST API server that provides inventory management functionality. It appears to be designed for a menu or catalog system, likely for a retail business in the cannabis industry based on the data model which includes fields like strain information, THC content, and effects.

## Technologies

The backend utilizes several key Haskell libraries and technologies:

- **Servant**: A type-level web framework that provides type-safe routing and API specifications
- **PostgreSQL**: Used as the primary database through the `postgresql-simple` library
- **Warp**: A high-performance HTTP server implementation
- **Resource Pooling**: Connection pooling for database access via the `resource-pool` library
- **CORS Support**: Cross-Origin Resource Sharing middleware for web compatibility
- **UUID**: Unique identifiers for inventory items
- **JSON**: Data serialization and deserialization using Aeson

## Architecture

The application follows a layered architecture approach:

1. **API Layer** (`API.hs`): Defines the RESTful API endpoints using Servant's type-level DSL
2. **Server Layer** (`Server.hs`): Implements the API handlers and routes requests to the appropriate business logic
3. **Database Layer** (`Database.hs`): Manages database connections and provides CRUD operations for the data model
4. **Application Core** (`App.hs`): Configures and bootstraps the application, setting up middleware and server options
5. **Types** (`Types.hs`): Defines the domain model and data types used throughout the application

## API Reference

The backend exposes a RESTful API for inventory management with the following endpoints:

### Inventory Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/inventory` | Retrieve all inventory items |
| POST | `/inventory` | Add a new inventory item |
| PUT | `/inventory` | Update an existing inventory item |
| DELETE | `/inventory/:sku` | Delete an inventory item by SKU (UUID) |

All endpoints use JSON for request and response bodies.

### Response Format

All API responses follow a consistent format:

```json
{
  "type": "data" | "message",
  "value": <Inventory array or message text>
}
```

## Database Schema

The application uses two primary tables in PostgreSQL:

### menu_items Table

Stores the base information about each menu item:

| Column | Type | Description |
|--------|------|-------------|
| sort | INT | Display order priority |
| sku | UUID | Primary key, unique identifier |
| brand | TEXT | Product brand name |
| name | TEXT | Product name |
| price | DECIMAL(10,2) | Product price |
| measure_unit | TEXT | Unit of measurement (e.g., "g", "oz") |
| per_package | TEXT | Amount per package |
| quantity | INT | Available quantity |
| category | TEXT | Product category (Flower, PreRolls, etc.) |
| subcategory | TEXT | Product subcategory |
| description | TEXT | Product description |
| tags | TEXT[] | Array of product tags |
| effects | TEXT[] | Array of effects the product has |

### strain_lineage Table

Stores detailed strain information with a foreign key relationship to menu_items:

| Column | Type | Description |
|--------|------|-------------|
| sku | UUID | Primary key and foreign key to menu_items |
| thc | TEXT | THC content information |
| cbg | TEXT | CBG content information |
| strain | TEXT | Strain name |
| creator | TEXT | Strain creator/breeder |
| species | TEXT | Species classification |
| dominant_terpene | TEXT | Primary terpene |
| terpenes | TEXT[] | Array of all terpenes |
| lineage | TEXT[] | Array of ancestor strains |
| leafly_url | TEXT | Link to Leafly information page |
| img | TEXT | Image URL |

## Data Types

The backend defines several key data types:

### MenuItem

Represents a complete menu item with all its properties, including:
- Basic product information (SKU, name, price, etc.)
- Categorization data
- Strain lineage details (as a nested structure)

### StrainLineage

Contains detailed information about a cannabis strain, including:
- Cannabinoid content (THC, CBG)
- Strain information and genetics
- Terpene profile
- Species classification

### ItemCategory

An enumeration of possible product categories:
- Flower
- PreRolls
- Vaporizers
- Edibles
- Drinks
- Concentrates
- Topicals
- Tinctures
- Accessories

### Species

An enumeration of cannabis species classifications:
- Indica
- IndicaDominantHybrid
- Hybrid
- SativaDominantHybrid
- Sativa

### Inventory

A collection of menu items, represented as a vector for efficient operations.

### InventoryResponse

A unified response type that can either contain:
- Full inventory data
- A status message (success, error, etc.)

## Implementation Approaches

The backend employs several noteworthy implementation approaches:

### Database Connection Management

- **Connection Pooling**: Uses a connection pool for efficient database access
- **Retry Logic**: Implements connection retries with exponential backoff on database connection failures
- **Resource Safety**: Ensures connections are properly closed with `withConnection` patterns

### Error Handling

- **Structured Error Responses**: Consistent error message format for client consumption
- **Exception Catching**: Try/catch patterns to handle exceptions gracefully
- **Logging**: Console logging of errors and important events

### CORS Configuration

The application implements a permissive CORS policy to allow browser access from any origin, which is suitable for development but may need to be restricted in production:

```haskell
CorsResourcePolicy
  { corsOrigins = Nothing  -- Allow any origin
  , corsMethods = [methodGet, methodPost, methodPut, methodDelete, methodOptions]
  , corsRequestHeaders = [hContentType, hAccept, hAuthorization, hOrigin, hContentLength]
  , corsExposedHeaders = Nothing
  , corsMaxAge = Just 3600
  , corsVaryOrigin = False
  , corsRequireOrigin = False
  , corsIgnoreFailures = False
  }
```

### Database Operations

Database operations are implemented with the following approaches:

- **Query Interpolation**: Uses the `sql` quasi-quoter for readable SQL queries
- **Transaction Safety**: Ensures database operations maintain consistency
- **Type Conversion**: Custom `FromRow` and `ToRow` instances to convert between Haskell types and database representations
- **Array Handling**: Special handling for PostgreSQL arrays with the `PGArray` type

## Development Guidelines

### Environment Setup

1. Ensure PostgreSQL is running using the provided Nix development environment
2. Run `pg-start` to start the database
3. Execute `pg-create-schema` to set up the required tables if needed

### Server Configuration

The server is configured with the following default settings:

- **Port**: 8080
- **Database**: localhost:5432/cheeblr
- **Username**: Current system user (obtained via `getLoginName`)
- **Password**: "postgres"
- **Connection Pool Size**: 10

### Building and Running

Build the project with:
```bash
cabal build
```

Run the server with:
```bash
cabal run
```

When started, the server will:
1. Initialize the database connection pool
2. Create tables if they don't exist
3. Start listening on all interfaces on port 8080

## Security Considerations

The current implementation has several security aspects to be aware of:

1. **Hardcoded Password**: The database password is hardcoded as "postgres" in the configuration
2. **Open CORS Policy**: The server allows requests from any origin
3. **Error Messages**: Detailed error messages may expose internal information

For production deployment, consider:
- Using environment variables for sensitive configuration
- Restricting CORS to specific origins
- Sanitizing error messages sent to clients
- Implementing authentication and authorization