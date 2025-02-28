# Cheeblr Frontend Documentation

This document provides detailed information about the PureScript frontend for the Cheeblr application.

## Table of Contents
- [Overview](#overview)
- [Technologies](#technologies)
- [Architecture](#architecture)
- [Components](#components)
- [Routing](#routing)
- [State Management](#state-management)
- [API Integration](#api-integration)
- [Form Handling](#form-handling)
- [Validation](#validation)
- [Development Guidelines](#development-guidelines)
- [Deployment](#deployment)

## Overview

The Cheeblr frontend is a single-page application (SPA) built with PureScript. It provides a user interface for managing an inventory of cannabis products, allowing users to view, create, edit, and delete inventory items. The application connects to a Haskell backend API to perform these operations.

## Technologies

The frontend utilizes several PureScript libraries and technologies:

- **Deku**: A declarative UI library for PureScript with hooks-like functionality
- **FRP**: Functional Reactive Programming principles for state management
- **Routing**: Client-side routing using hash-based navigation
- **Fetch**: HTTP requests to the backend API
- **JSON**: Data serialization and deserialization
- **Validation**: Form validation with semigroup-based validation

## Architecture

The application follows a component-based architecture pattern:

1. **Main Module**: The entry point that sets up routing and global state
2. **Components**: UI components for different views (LiveView, Create, Edit, Delete)
3. **API Module**: Handles communication with the backend
4. **Types**: Shared domain models and type definitions
5. **Validation**: Form validation logic and rules
6. **Utilities**: Helper functions and utilities

## Components

### MenuLiveView

The `MenuLiveView` component displays the inventory items in a grid layout. It:
- Fetches inventory data from the backend
- Displays loading states and error messages
- Renders inventory items with sorting and filtering capabilities
- Provides links to edit and delete items

### CreateItem

The `CreateItem` component provides a form for adding new inventory items. It:
- Generates a UUID for new items
- Implements comprehensive form validation
- Submits new items to the backend API
- Provides feedback on submission status

### EditItem

The `EditItem` component allows modification of existing inventory items. It:
- Loads the selected item's data
- Populates the form with current values
- Validates user input
- Submits updates to the backend API

### DeleteItem

The `DeleteItem` component provides confirmation before deleting an item. It:
- Displays item details to confirm deletion
- Handles the delete API request
- Shows success or error messages
- Provides navigation back to the inventory list

## Routing

The application uses hash-based routing with the following routes:

- `/` - Main inventory view (LiveView)
- `/create` - Create new item form
- `/edit/:uuid` - Edit an existing item by UUID
- `/delete/:uuid` - Delete confirmation for an item by UUID

Routes are defined in the `Route` module and parsed using `routing-duplex`. The `Main` module sets up route matching and component rendering based on the current route.

## State Management

The application uses Functional Reactive Programming (FRP) principles for state management:

- **Global State**: The `Main` module maintains global state like current inventory, loading status, and errors
- **Component State**: Each component manages its own local state using Deku's hooks system (`useState`)
- **Event Handling**: Events are handled using Deku's event system
- **Data Flow**: Unidirectional data flow from state to UI

## API Integration

The `API` module provides functions for interacting with the backend:

### Endpoints

- `readInventory`: Fetches all inventory items
- `writeInventory`: Creates a new inventory item
- `updateInventory`: Updates an existing inventory item
- `deleteInventory`: Deletes an inventory item by ID

The API module handles:
- Constructing API requests with proper headers
- Serializing data to JSON
- Deserializing responses
- Error handling

## Form Handling

Forms are implemented using a combination of:

1. **Field Components**: Reusable form field components defined in the `Form` module
2. **State Hooks**: Each form field has associated state using Deku's `useState` hook
3. **Validation**: Real-time validation with feedback
4. **Form Submission**: Collecting and validating all fields before submission

### Field Types

The application supports various field types:

- Text fields
- Number fields
- Dropdown selects (for categories and species)
- Textarea for longer text

## Validation

The `Validation` module provides robust form validation:

### Validation Rules

- `nonEmpty`: Ensures a field is not empty
- `alphanumeric`: Validates text contains only letters, numbers, and spaces
- `extendedAlphanumeric`: More permissive alphanumeric that allows additional characters
- `percentage`: Validates percentage format (e.g., "20.5%")
- `dollarAmount`: Validates currency amounts
- `validMeasurementUnit`: Validates units of measurement
- `validUrl`: Validates URL format
- `nonNegativeInteger`: Validates non-negative whole numbers
- `commaList`: Validates comma-separated lists

### Validation Approach

The validation system uses a semigroup-based approach with the `V` type from `Data.Validation.Semigroup`, allowing for collecting multiple validation errors across the form. The final validation step converts this to an `Either String MenuItem` result.

## Data Types

### Core Data Types

- **MenuItem**: Represents a complete menu item with all associated data
- **StrainLineage**: Contains strain-specific information
- **ItemCategory**: Enumeration of product categories
- **Species**: Enumeration of cannabis species
- **Inventory**: Collection of menu items
- **InventoryResponse**: API response type that can contain inventory data or a status message

## Network Configuration

The application includes environment-specific network configuration:

- **Local Development**: Configuration for local development environment
- **Network Development**: Configuration for network-based development environment

The `currentConfig` setting determines which environment is used.

## Development Guidelines

### Project Setup

1. Use the Nix development environment to ensure consistent tooling
2. Start the frontend development server using the `dev` command from the Nix shell

### Component Development

When creating new components:

1. Define the component in its own module
2. Use Deku hooks for state management
3. Implement proper validation for any forms
4. Handle loading and error states
5. Include debug information during development

### API Interaction

When making API calls:

1. Use the functions in the `API` module
2. Handle both success and error responses
3. Provide user feedback during loading and after completion
4. Log details to the console for debugging

## Debug Features

The application includes several debug features:

- **Console Logging**: Extensive console logging for tracking component lifecycle and API interactions
- **Debug Information**: UI components display debug information about their state
- **Form Validation Display**: Visual feedback on which form fields are valid/invalid

## UI/UX Considerations

The frontend implements several UX patterns:

- **Loading States**: Visual feedback during data loading
- **Error Messages**: Clear error messaging when issues occur
- **Form Validation**: Real-time validation feedback
- **Responsive Design**: Grid-based layout that adapts to different screens

## Implementation Approaches

### Form Building

The application takes a compositional approach to form building:

1. **Field Configuration**: Each field type has a configuration object defining its behavior
2. **Field Generation**: The `makeField` and `makeDropdown` functions generate form fields
3. **Validation Rules**: Validation rules are composed using `allOf` and `anyOf` combinators
4. **State Tracking**: Each field's state (value and validity) is tracked independently

### Inventory Display

The `MenuLiveView` component implements several display features:

1. **Sorting**: Items can be sorted by various fields
2. **Filtering**: Out-of-stock items can be hidden
3. **Visual Categorization**: Items are visually distinguished by category and species
4. **Interactive Elements**: Edit and delete actions are accessible from each item card

### Error Handling

The application implements comprehensive error handling:

1. **API Errors**: Errors from API calls are caught and displayed
2. **Form Validation**: Form errors are displayed inline
3. **Not Found**: Missing items trigger appropriate error displays
4. **Network Issues**: Network-related errors are handled and reported to the user

## Performance Considerations

The frontend implementation includes several performance optimizations:

1. **Lazy Loading**: Components are loaded only when needed
2. **Efficient Updates**: Deku's efficient update mechanism minimizes DOM operations
3. **Validation Timing**: Validation runs on input change rather than continuously
4. **Debounced Operations**: Rate-limited operations prevent excessive API calls