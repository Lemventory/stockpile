# Cheeblr Dependencies

This document lists all dependencies used in the Cheeblr project, including both the PureScript frontend and Haskell backend.

## Frontend Dependencies (PureScript)

| Package | Version | Description |
|---------|---------|-------------|
| aff | 7.1.0 | Asynchronous effect monad |
| aff-promise | 4.0.0 | Bridge between JavaScript promises and PureScript Aff |
| affjax | 13.0.0 | AJAX requests using Aff |
| affjax-web | 1.0.0 | Web-specific AJAX functionality |
| arraybuffer | 13.2.0 | ArrayBuffer operations |
| arraybuffer-types | 3.0.2 | Types for JavaScript ArrayBuffers |
| arrays | 7.3.0 | Array operations |
| console | 6.1.0 | Console-based logging |
| css | 6.0.0 | CSS handling |
| debug | 6.0.2 | Debugging utilities |
| deku-core | git | Core of the Deku UI framework |
| deku-css | git | CSS integration for Deku |
| deku-dom | git | DOM bindings for Deku |
| effect | 4.0.0 | Side-effect handling |
| either | 6.1.0 | Either type |
| encoding | 0.0.9 | Encoding utilities |
| enums | 6.0.1 | Enumeration utilities |
| fetch | 4.1.0 | Fetch API bindings |
| fetch-yoga-json | 1.1.0 | JSON handling for Fetch |
| hyrule | git | Reactive programming library |
| lists | 7.0.0 | Linked list data structure |
| maybe | 6.0.0 | Optional value handling |
| newtype | 5.0.0 | Newtype utilities |
| node-fs | 9.2.0 | Node.js filesystem operations |
| parsing | 10.2.0 | Parser combinators |
| prelude | 6.0.1 | Standard library |
| routing | 11.0.0 | Client-side routing |
| routing-duplex | 0.7.0 | Bi-directional routing |
| transformers | 6.1.0 | Monad transformers |
| tuples | 7.0.0 | Tuple data structures |
| untagged-union | 1.0.0 | Union types without tags |
| unsafe-reference | 5.0.0 | Unsafe reference operations |
| uuid | 9.0.0 | UUID generation and handling |
| validation | 6.0.0 | Validation utilities |
| web-html | 4.1.0 | Web HTML bindings |
| web-pointerevents | 2.0.0 | Pointer events bindings |
| web-touchevents | 4.0.0 | Touch events bindings |
| web-uievents | 5.0.0 | UI events bindings |
| yoga-json | 5.1.0 | JSON parsing and serialization |
| yoga-tree | 1.0.0 | Tree data structure |

### Development Dependencies (PureScript)

| Package | Version | Description |
|---------|---------|-------------|
| tidy | v0.10.0 | PureScript code formatter |
| tidy-codegen | main | Code generation utilities |
| dodo-printer | master | Pretty-printing library |

## Backend Dependencies (Haskell)

| Package | Version | Description |
|---------|---------|-------------|
| aeson | 2.2.3.0 | JSON serialization and deserialization |
| base | 4.18.2.1 | Haskell standard library |
| bytestring | 0.11.5.3 | Efficient byte strings |
| http-types | 0.12.4 | HTTP types |
| mtl | 2.3.1 | Monad transformer library |
| opaleye | 0.10.5.0 | SQL generation for PostgreSQL |
| postgresql-libpq | 0.11.0.0 | Low-level bindings to PostgreSQL |
| postgresql-simple | 0.7.0.0 | Simple PostgreSQL interface |
| refined | 0.8.2 | Refinement types |
| resource-pool | 0.4.0.0 | Resource pooling, used for DB connections |
| scientific | 0.3.8.0 | Scientific number type |
| servant | 0.20.2 | Type-level web API library |
| servant-server | 0.20.2 | Server implementation for Servant |
| string-conversions | 0.4.0.1 | String conversion utilities |
| text | 2.0.2 | Efficient text type |
| time | 1.12.2 | Time and date types and functions |
| transformers | 0.6.1.0 | Monad transformers |
| unix | 2.8.4.0 | UNIX utilities |
| uuid | 1.3.16 | UUID type and utilities |
| vector | 0.13.2.0 | Efficient arrays |
| wai | 3.2.4 | Web application interface |
| wai-cors | 0.2.7 | CORS for WAI applications |
| warp | 3.4.7 | HTTP server library |

## Dependency Graph

The Cheeblr application is built with the following high-level dependency structure:

```
Cheeblr
├── Frontend (PureScript)
│   ├── Deku (UI Framework)
│   │   ├── deku-core
│   │   ├── deku-dom
│   │   └── deku-css
│   ├── Hyrule (Reactive Programming)
│   ├── Routing
│   └── API Communication
│       ├── fetch
│       └── yoga-json
│
└── Backend (Haskell)
    ├── Servant (API Framework)
    ├── PostgreSQL
    │   ├── postgresql-simple
    │   └── resource-pool
    └── Warp (HTTP Server)
```

## Key Dependencies Explained

### Frontend

- **Deku**: A reactive UI framework for PureScript, similar to React but with more functional reactive programming principles
- **Hyrule**: A library for reactive programming, used by Deku for state management
- **Routing**: Client-side routing using hash-based navigation
- **Fetch**: Modern API for making HTTP requests
- **Yoga-JSON**: JSON handling library with good type safety

### Backend

- **Servant**: Type-level web API framework that provides type-safe routing
- **PostgreSQL-Simple**: Library for connecting to and querying PostgreSQL databases
- **Resource-Pool**: Connection pooling for database connections
- **Warp**: Fast, production-quality HTTP server
- **WAI-CORS**: Cross-Origin Resource Sharing middleware for web applications