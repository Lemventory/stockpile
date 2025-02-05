{ config, lib, pkgs, name, ... }:

with lib;
let
  cfg = config.services.${pgConfig.database.name}.postgresql;
  pgConfig = import ./postgresql-config.nix;

in {

  options.services.${pgConfig.database.name}.postgresql = {
    enable = mkEnableOption "Cheeblr PostgreSQL Service";
    package = mkOption {
      type = types.package;
      default = pkgs.postgresql;
      description = "PostgreSQL package to use";
    };
    port = mkOption {
      type = types.port;
      default = pgConfig.database.port;
      description = "PostgreSQL port number";
    };
    dataDir = mkOption {
      type = types.str;
      default = "/var/lib/postgresql/${config.services.postgresql.package.psqlSchema}";
      description = "PostgreSQL data directory";
    };
  };

  config = mkIf cfg.enable {
    services.postgresql = {
      enable = true;
      package = cfg.package;
      enableTCPIP = true;
      port = cfg.port;
      dataDir = cfg.dataDir;
      ensureDatabases = [ pgConfig.database.name ];
      
      authentication = pkgs.lib.mkOverride 10 ''
        # Local connections use password
        local   all             all                                     trust
        # Allow localhost TCP connections with password
        host    all             all             127.0.0.1/32           trust
        host    all             all             ::1/128                trust
      '';

      initialScript = pkgs.writeText "${pgConfig.database.name}-init" ''
        DO $$
        BEGIN
          IF NOT EXISTS (SELECT FROM pg_user WHERE usename = '${pgConfig.database.user}') THEN
            CREATE USER ${pgConfig.database.user} WITH PASSWORD '${pgConfig.database.password}' SUPERUSER;
          END IF;
        END
        $$;

        CREATE DATABASE ${pgConfig.database.name};
        GRANT ALL PRIVILEGES ON DATABASE ${pgConfig.database.name} TO ${pgConfig.database.user};
      '';

      settings = {
        # Default config
        max_connections = 100;
        shared_buffers = "128MB";
        dynamic_shared_memory_type = "posix";
        log_destination = "stderr";
        logging_collector = true;
        log_directory = "log";
        log_filename = "postgresql-%Y-%m-%d_%H%M%S.log";
        log_min_messages = "info";
        log_min_error_statement = "info";
        log_connections = true;
      };
    };

    environment.systemPackages = [ cfg.package ];
    
    environment.variables = {
      PGHOST = "localhost";
      PGPORT = toString cfg.port;
      PGUSER = pgConfig.database.user;
      PGDATABASE = pgConfig.database.name;
      DATABASE_URL = "postgresql://${pgConfig.database.user}:${pgConfig.database.password}@localhost:${toString cfg.port}/${pgConfig.database.name}";
    };
  };
}