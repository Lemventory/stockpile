# Cheeblr Dependencies

This document lists all dependencies used in the Cheeblr project, including both the PureScript frontend and Haskell backend.

## Frontend Dependencies (PureScript)

| Package | Version/Source | Description |
|---------|----------------|-------------|
| aff | 7.1.0 | Asynchronous effect monad |
| aff-promise | 4.0.0 | Bridge between JavaScript promises and PureScript Aff |
| affjax | 13.0.0 | AJAX requests using Aff |
| affjax-web | 1.0.0 | Web-specific AJAX functionality |
| arraybuffer | 13.2.0 | ArrayBuffer operations |
| arraybuffer-types | 3.0.2 | Types for JavaScript ArrayBuffers |
| arrays | 7.3.0 | Array operations |
| console | 6.1.0 | Console-based logging |
| css | 6.0.0 | CSS handling |
| datetime | 6.1.0 | Date and time operations |
| debug | 6.0.2 | Debugging utilities |
| deku-core | GitHub - mikesol/purescript-deku | Core of the Deku UI framework |
| deku-css | GitHub - mikesol/purescript-deku | CSS integration for Deku |
| deku-dom | GitHub - mikesol/purescript-deku | DOM bindings for Deku |
| effect | 4.0.0 | Side-effect handling |
| either | 6.1.0 | Either type |
| encoding | 0.0.9 | Encoding utilities |
| enums | 6.0.1 | Enumeration utilities |
| fetch | 4.1.0 | Fetch API bindings |
| fetch-yoga-json | 1.1.0 | JSON handling for Fetch |
| lists | 7.0.0 | Linked list data structure |
| maybe | 6.0.0 | Optional value handling |
| money | GitHub - harryprayiv/purescript-money | Money and currency handling |
| newtype | 5.0.0 | Newtype utilities |
| node-fs | 9.2.0 | Node.js filesystem operations |
| numbers | 9.0.1 | Numeric type operations |
| parsing | 10.2.0 | Parser combinators |
| prelude | 6.0.1 | PureScript standard library |
| routing | 11.0.0 | Client-side routing |
| routing-duplex | 0.7.0 | Bi-directional routing |
| transformers | 6.1.0 | Monad transformers |
| tuples | 7.0.0 | Tuple data structures |
| uuid | 9.0.0 | UUID generation and handling |
| validation | 6.0.0 | Validation utilities |
| web-html | 4.1.0 | Web HTML bindings |
| yoga-json | 5.1.0 | JSON parsing and serialization |

## Backend Dependencies (Haskell)

| Package | Version | Description |
|---------|---------|-------------|
| aeson | 2.2.3.0 | JSON serialization and deserialization |
| base | 4.18.2.1 | Haskell standard library |
| base-compat | 0.14.1 | Compatibility layer for base |
| blaze-html | 0.9.2.0 | HTML templating library |
| blaze-markup | 0.8.3.0 | Markup combinator library |
| bytestring | 0.11.5.3 | Efficient byte strings |
| case-insensitive | 1.2.1.0 | Case insensitive string comparison |
| containers | 0.6.7 | Efficient container data structures |
| cookie | 0.5.0 | HTTP cookie handling |
| data-default | 0.8.0.0 | Default value typeclass |
| http-api-data | 0.6.1 | Conversion between HTTP API data and Haskell types |
| http-types | 0.12.4 | HTTP types |
| mtl | 2.3.1 | Monad transformer library |
| network | 3.2.7.0 | Low-level networking interface |
| postgresql-libpq | 0.11.0.0 | Low-level bindings to PostgreSQL |
| postgresql-simple | 0.7.0.0 | Simple PostgreSQL interface |
| resource-pool | 0.4.0.0 | Resource pooling, used for DB connections |
| scientific | 0.3.8.0 | Scientific number type |
| servant | 0.20.2 | Type-level web API library |
| servant-server | 0.20.2 | Server implementation for Servant |
| stm | 2.5.1.0 | Software transactional memory |
| text | 2.0.2 | Efficient text type |
| time | 1.12.2 | Time and date types and functions |
| uuid | 1.3.16 | UUID type and utilities |
| vector | 0.13.2.0 | Efficient arrays |
| wai | 3.2.4 | Web application interface |
| wai-app-static | 3.1.9 | Static file serving for WAI applications |
| wai-cors | 0.2.7 | CORS for WAI applications |
| wai-extra | 3.1.17 | Additional WAI utilities |
| wai-logger | 2.5.0 | Logging middleware for WAI |
| warp | 3.4.7 | HTTP server library |

## Specialized Libraries

### Finance Support (Frontend)

The frontend uses specialized financial libraries:

- **purescript-money**: Provides accurate currency representation and operations
- **Data.Finance.Currency**: Represents currency types with proper handling
- **Data.Finance.Money**: Includes the `Discrete` type for precise money representation

### Database Interaction (Backend)

The backend uses PostgreSQL-specific libraries:

- **postgresql-simple**: High-level interface to PostgreSQL
- **postgresql-libpq**: Low-level PostgreSQL bindings
- **resource-pool**: Connection pooling for database efficiency

## Dependency Graph

The Cheeblr application is built with the following high-level dependency structure:

```
Cheeblr
├── Frontend (PureScript)
│   ├── Deku (UI Framework)
│   │   ├── deku-core
│   │   ├── deku-dom
│   │   └── deku-css
│   ├── State Management
│   │   └── FRP.Poll
│   ├── Routing
│   │   └── routing-duplex
│   ├── API Communication
│   │   ├── fetch
│   │   └── yoga-json
│   └── Financial Operations
│       └── purescript-money
│
└── Backend (Haskell)
    ├── Servant (API Framework)
    ├── PostgreSQL
    │   ├── postgresql-simple
    │   └── resource-pool
    ├── Warp (HTTP Server)
    └── WAI Middleware
        ├── wai-cors
        └── wai-logger
```

## Key Dependencies Explained

### Frontend

- **Deku**: A reactive UI framework for PureScript that provides component-based architecture
- **FRP.Poll**: Functional reactive programming primitives for state management
- **Routing-Duplex**: Bi-directional routing library to handle client-side navigation
- **Fetch & Yoga-JSON**: Network request handling and JSON processing
- **PureScript-Money**: Specializes in handling financial calculations with proper decimal precision

### Backend

- **Servant**: Type-level web API framework that provides type-safe routing and API definitions
- **PostgreSQL-Simple**: Library for connecting to and querying PostgreSQL databases
- **Resource-Pool**: Connection pooling for database connections
- **Warp**: Fast, production-quality HTTP server
- **WAI-CORS**: Cross-Origin Resource Sharing middleware for web applications