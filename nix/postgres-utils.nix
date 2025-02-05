{ pkgs
, lib ? pkgs.lib
, name ? "cheeblr"
}: 

let
  pgConfig = import ./postgresql-config.nix { };
  
  postgresql = pkgs.postgresql;
  bin = {
    pgctl = "${postgresql}/bin/pg_ctl";
    psql = "${postgresql}/bin/psql";
    initdb = "${postgresql}/bin/initdb";
    createdb = "${postgresql}/bin/createdb";
    pgIsReady = "${postgresql}/bin/pg_isready";
  };

  config = {
    dataDir = pgConfig.database.dataDir;
    port = pgConfig.database.port;
    user = pgConfig.database.user;
    password = pgConfig.database.password;
  };

  mkPgConfig = ''
listen_addresses = '${pgConfig.database.settings.listen_addresses}'
port = ${toString config.port}
unix_socket_directories = '$PGDATA'
max_connections = ${toString pgConfig.database.settings.max_connections}
shared_buffers = '${pgConfig.database.settings.shared_buffers}'
dynamic_shared_memory_type = '${pgConfig.database.settings.dynamic_shared_memory_type}'
log_destination = 'stderr'
logging_collector = off
'';

  mkHbaConfig = ''
local   all             all                                     trust
host    all             all             127.0.0.1/32           trust
host    all             all             ::1/128                trust
'';

  envSetup = ''
    export PGPORT="''${PGPORT:-${toString config.port}}"
    export PGUSER="''${PGUSER:-${config.user}}"
    export PGDATABASE="''${PGDATABASE:-${pgConfig.database.name}}"
    export PGHOST="$PGDATA"
  '';

  validateEnv = ''
    if [ -z "$PGDATA" ]; then
      echo "Error: PGDATA environment variable must be set"
      exit 1
    fi
  '';

in {
  inherit config;

  setupScript = pkgs.writeShellScriptBin "pg-setup" ''
    ${envSetup}
    ${validateEnv}

    init_database() {
      echo "Creating PGDATA directory at: $PGDATA"
      rm -rf "$PGDATA"
      mkdir -p "$PGDATA"

      echo "Initializing database..."
      ${bin.initdb} -D "$PGDATA" \
            --auth=trust \
            --no-locale \
            --encoding=UTF8 \
            --username="${config.user}"

      # Write config files exactly as in working version
      cat > "$PGDATA/postgresql.conf" << EOF
${mkPgConfig}
EOF

      cat > "$PGDATA/pg_hba.conf" << EOF
${mkHbaConfig}
EOF
    }

    start_database() {
      echo "Starting PostgreSQL..."
      ${bin.pgctl} -D "$PGDATA" -l "$PGDATA/postgresql.log" start

      if [ $? -ne 0 ]; then
        echo "PostgreSQL failed to start. Here's the log:"
        cat "$PGDATA/postgresql.log"
        return 1
      fi

      echo "Waiting for PostgreSQL to be ready..."
      RETRIES=0
      while ! ${bin.pgIsReady} -h "$PGHOST" -p "$PGPORT" -q; do
        RETRIES=$((RETRIES+1))
        if [ $RETRIES -eq 10 ]; then
          echo "PostgreSQL failed to become ready. Here's the log:"
          cat "$PGDATA/postgresql.log"
          return 1
        fi
        sleep 1
        echo "Still waiting... (attempt $RETRIES/10)"
      done
    }

    setup_database() {
      echo "Creating database..."
      ${bin.createdb} -h "$PGHOST" -p "$PGPORT" "$PGDATABASE"
      
      if [ $? -ne 0 ]; then
        echo "Failed to create database"
        return 1
      fi

      # Use DO block for conditional user creation
      ${bin.psql} -h "$PGHOST" -p "$PGPORT" "$PGDATABASE" << EOF
DO \$\$
BEGIN
  IF NOT EXISTS (SELECT FROM pg_user WHERE usename = '${config.user}') THEN
    CREATE USER ${config.user} WITH PASSWORD '${config.password}' SUPERUSER;
  END IF;
END
\$\$;
GRANT ALL PRIVILEGES ON DATABASE ${pgConfig.database.name} TO ${config.user};
EOF
    }

    cleanup() {
      if [ -f "$PGDATA/postmaster.pid" ]; then
        echo "Stopping PostgreSQL..."
        ${bin.pgctl} -D "$PGDATA" stop -m fast
      fi
    }

    trap cleanup EXIT

    init_database && start_database && setup_database

    echo "Development environment ready:"
    echo "  Socket directory: $PGHOST"
    echo "  Port: $PGPORT"
    echo "  Database URL: postgresql://${config.user}:${config.password}@localhost:$PGPORT/$PGDATABASE"
    echo ""
    echo "You can connect to the database using:"
    echo "  ${bin.psql} -h $PGHOST -p $PGPORT $PGDATABASE"
  '';

  pg-start = pkgs.writeShellScriptBin "pg-start" ''
    ${envSetup}
    ${validateEnv}

    echo "Starting PostgreSQL..."
    ${bin.pgctl} -D "$PGDATA" -l "$PGDATA/postgresql.log" start

    if [ $? -ne 0 ]; then
      echo "PostgreSQL failed to start. Here's the log:"
      cat "$PGDATA/postgresql.log"
      exit 1
    fi

    echo "Waiting for PostgreSQL to be ready..."
    RETRIES=0
    while ! ${bin.pgIsReady} -h "$PGHOST" -p "$PGPORT" -q; do
      RETRIES=$((RETRIES+1))
      if [ $RETRIES -eq 10 ]; then
        echo "PostgreSQL failed to become ready. Here's the log:"
        cat "$PGDATA/postgresql.log"
        exit 1
      fi
      sleep 1
      echo "Still waiting... (attempt $RETRIES/10)"
    done
  '';

  pg-connect = pkgs.writeShellScriptBin "pg-connect" ''
    ${envSetup}
    ${validateEnv}

    if [ -z "$PGPORT" ]; then
      echo "Port must be set"
      exit 1
    fi
    if [ -z "$PGDATABASE" ]; then
      echo "Database name must be set"
      exit 1
    fi
    ${bin.psql} -h $PGHOST -p $PGPORT $PGDATABASE
  '';

  pg-stop = pkgs.writeShellScriptBin "pg-stop" ''
    ${envSetup}
    ${validateEnv}
    ${bin.pgctl} -D "$PGDATA" stop -m fast
  '';
}