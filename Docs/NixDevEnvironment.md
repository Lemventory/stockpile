# Cheeblr Development Environment

This document describes the Nix-based development environment for the Cheeblr project.

## Table of Contents

- [Overview](#overview)
- [Prerequisites](#prerequisites)
- [Getting Started](#getting-started)
- [Environment Components](#environment-components)
  - [Database](#database)
  - [Frontend](#frontend)
  - [Development Tools](#development-tools)
  - [Deployment](#deployment)
- [Common Workflows](#common-workflows)
- [Troubleshooting](#troubleshooting)

## Overview

Cheeblr is a project built with a Haskell backend and PureScript frontend. The development environment is managed through Nix flakes, providing a reproducible and consistent setup across different machines and operating systems.

## Prerequisites

- [Nix package manager](https://nixos.org/download.html) with flakes enabled
- Supported operating systems:
  - Linux (x86_64)
  - macOS (Intel or Apple Silicon)

## Getting Started

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd cheeblr
   ```

2. Enter the development shell:
   ```bash
   nix develop
   ```

3. Start the PostgreSQL database:
   ```bash
   pg-start
   ```

4. Start the development environment:
   ```bash
   dev
   ```

## Environment Components

### Database

The project uses PostgreSQL for data storage. The following commands are available to manage the database:

| Command | Description |
|---------|-------------|
| `pg-start` | Start the PostgreSQL server |
| `pg-connect` | Connect to the PostgreSQL server with an interactive CLI |
| `pg-stop` | Stop the PostgreSQL server |
| `pg-cleanup` | Clean up PostgreSQL data directory |
| `pg-backup` | Create a backup of the database |
| `pg-restore` | Restore a backup of the database |
| `pg-rotate-credentials` | Change database credentials |
| `pg-create-schema` | Create database schema |
| `pg-stats` | Show database statistics |

The PostgreSQL data is stored in `$HOME/.local/share/cheeblr/postgres`. The default connection parameters are:

- Port: 5432
- User: Your system username
- Password: "postgres"
- Database: "cheeblr"

### Frontend

The frontend is built with PureScript. The following commands are available for frontend development:

| Command | Description |
|---------|-------------|
| `vite` | Start the Vite dev server |
| `vite-cleanup` | Clean up Vite build artifacts |
| `spago-watch` | Watch PureScript files and recompile on changes |
| `concurrent` | Run multiple commands concurrently |
| `dev` | Start the full development environment |

### Development Tools

The environment includes various development tools:

- **Haskell**: GHC, Cabal, HLS, Fourmolu, HLint
- **PureScript**: PureScript compiler, Spago, PureScript Language Server, PureScript ES backend, PureScript tidy
- **JavaScript/Node.js**: Node.js 20, esbuild
- **Database**: PostgreSQL, pgcli, pgadmin4
- **Editors**: VSCodium (via `code-workspace` command)
- **Other**: tmux, rsync, nixpkgs-fmt

### Deployment

The environment includes deployment tools:

| Command | Description |
|---------|-------------|
| `deploy` | Deploy the application |
| `withdraw` | Withdraw the application (or funds) |

## Common Workflows

### Start Development Session

```bash
nix develop  # Enter the dev environment
pg-start     # Start the database
dev          # Start the development servers
```

### Database Management

```bash
pg-connect    # Connect to database with CLI
pg-backup     # Create a backup
pg-restore    # Restore from backup
```

### Backup Project

```bash
backup-project  # Backup project files and database
```

This will:
- Copy project files to `~/plutus/workspace/scdWs/cheeblr/`
- Copy database backups to `~/plutus/cheeblrDB/`

### Open in VSCodium

```bash
code-workspace  # Open the project in VSCodium
```

## Troubleshooting

### Database Issues

If you encounter issues with the PostgreSQL server:

1. Stop the server: `pg-stop`
2. Clean up: `pg-cleanup`
3. Start again: `pg-start`
4. Create schema: `pg-create-schema`

### Build Issues

For frontend build issues:

1. Clean up Vite artifacts: `vite-cleanup`
2. Restart development: `dev`

## Project Structure

The project appears to be structured with:

- Haskell backend (likely using the Cardano ecosystem given the dependencies)
- PureScript frontend
- PostgreSQL database
- Nix-based configuration for reproducible development and deployment

## NixOS Integration

This project includes NixOS modules that can be imported in your NixOS configuration:

```nix
# configuration.nix
{ pkgs, ... }:
{
  imports = [ 
    (builtins.getFlake "github:your-username/cheeblr").nixosModules.default
  ];
}
```

This will import the PostgreSQL service configuration for your system.