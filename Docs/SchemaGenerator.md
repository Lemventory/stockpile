# stockpile Schema Generator

This tool generates consistent type definitions, validation rules, and database schemas from a single Dhall configuration file. It ensures that your PureScript frontend and Haskell backend share a consistent data model.

## Overview

The Schema Generator creates:

1. **Haskell Types** - Data types with appropriate instances for Aeson, PostgreSQL, etc.
2. **PureScript Types** - Types with validation, newtypes, and proper instances
3. **SQL Schema** - Database tables, constraints, and indexes

## Getting Started

### Installation

Make sure you have the Dhall package installed:

```
cabal install dhall
```

### Running the Generator

The generator supports several commands:

```bash
# Generate a starter schema
./schema-generator starter

# Generate a starter schema at a custom location
./schema-generator starter /path/to/my-schema.dhall

# Generate code using the default schema
./schema-generator generate

# Generate code from a specific schema
./schema-generator generate /path/to/my-schema.dhall

# Generate code to a specific output directory
./schema-generator generate /path/to/schema.dhall /path/to/output

# Deploy generated code to your project
./schema-generator deploy
```

## Schema File Format

The schema file uses Dhall, a programmable configuration language. Here's the basic structure:

```dhall
let FieldType = 
  < FTText
  | FTInt
  | FTNatural
  | FTDouble
  | FTScientific
  | FTUuid
  | FTBool
  | FTEnum : Text
  | FTArray : FieldType
  | FTRecord : Text
  >

-- Field definition
let Field = { fieldName : Text, fieldType : FieldType, fieldRequired : Bool, fieldDescription : Text }

-- Entity (table/type) definition
let Entity = { entityName : Text, entityFields : List Field, entityPrimaryKey : List Text, entityDescription : Text }

-- Enum definition
let EnumDef = { enumName : Text, enumValues : List Text, enumDescription : Text }

-- Overall schema
let Schema = { schemaName : Text, schemaEntities : List Entity, schemaEnums : List EnumDef, schemaDescription : Text }

-- Define your enums
let speciesEnum = { enumName = "Species", enumValues = ["Indica", "Hybrid", "Sativa"], enumDescription = "..." }

-- Define your entities
let menuItemEntity = { entityName = "MenuItem", entityFields = [...], entityPrimaryKey = ["id"], entityDescription = "..." }

-- Complete schema
let mySchema : Schema = {
  schemaName = "MyApp",
  schemaDescription = "My application schema",
  schemaEnums = [speciesEnum, ...],
  schemaEntities = [menuItemEntity, ...]
}

in mySchema
```

## Type Mappings

| Dhall Type    | Haskell           | PureScript      | SQL              |
|---------------|-------------------|-----------------|------------------|
| FTText        | Text              | String          | TEXT             |
| FTInt         | Int               | Int             | INT              |
| FTScientific  | Scientific        | Number          | DECIMAL(10,2)    |
| FTUuid        | UUID              | UUID            | UUID             |
| FTBool        | Bool              | Boolean         | BOOLEAN          |
| FTEnum        | Custom Enum       | Custom Enum     | TEXT             |
| FTArray       | V.Vector a        | Array a         | TEXT[] or JSONB  |
| FTRecord      | Custom Type       | Custom Type     | JSONB            |

## Customizing Output

The generator creates appropriate serialization instances:

- In Haskell: ToJSON/FromJSON, ToRow/FromRow
- In PureScript: WriteForeign/ReadForeign, Show, FormValue, etc.

## Example: Inventory Management

The inventory management system defines:

1. **Enums**: Species, ItemCategory
2. **Entities**: StrainLineage, MenuItem, Inventory, InventoryResponse
3. **Field Types**: Text, Int, UUID, Arrays, etc.

This creates matching types in both Haskell and PureScript with appropriate validation and serialization.

## Project Structure

When running `schema-generator deploy`, the files will be copied to:
- `/home/bismuth/workdir/stockpile/backend/src/Types.hs`
- `/home/bismuth/workdir/stockpile/frontend/src/Types.purs`

The SQL schema will be available at:
- `/home/bismuth/workdir/stockpile/backend/codegen/generated/001_initial_schema.sql`