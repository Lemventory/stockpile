# Cheeblr: Cannabis Dispensary Management System

A comprehensive full-stack web application for cannabis dispensary inventory and point-of-sale management, utilizing PureScript for frontend development with Haskell backend services, all underpinned by a PostgreSQL database infrastructure.

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

- **Comprehensive Inventory Management**: Track cannabis products with detailed strain information
- **Type-Safe API**: Fully type-checked communication between frontend and backend
- **Responsive UI**: Modern user interface built with PureScript and Deku
- **PostgreSQL Database**: Robust data storage with relationship modeling
- **Multiple Deployment Options**: Configure for online or kiosk mode
- **Flexible Sorting & Filtering**: Advanced inventory organization capabilities
- **CRUD Operations**: Complete support for creating, reading, updating, and deleting inventory items

## 🔧 Technology Stack

### Frontend
- **PureScript**: Strongly-typed functional programming language that compiles to JavaScript
- **Deku**: Declarative UI library for PureScript with hooks-like functionality
- **FRP**: Functional Reactive Programming for state management

### Backend
- **Haskell**: Pure functional programming language for robust backend services
- **Servant**: Type-level web API library for defining type-safe endpoints
- **PostgreSQL**: Advanced open-source relational database

### Development Environment
- **Nix**: Reproducible development environment with all dependencies
- **Cabal**: Haskell build system
- **Spago**: PureScript package manager and build tool

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

## 🔄 Current Development Status

The system currently has:

- A complete inventory management interface
- Backend API for CRUD operations on inventory items
- PostgreSQL database integration
- Type-safe communication between frontend and backend
- Detailed strain and product information tracking


## 📜 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.