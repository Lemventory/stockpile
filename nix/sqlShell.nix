# sqlShell.nix
{ pkgs }:

{
  pg-stop = pkgs.writeShellScriptBin "pg-stop" ''
    pg_ctl -D $PGDATA -U postgres stop
  '';

  pg-reset = pkgs.writeShellScriptBin "pg-reset" ''
    rm -rf $PGDATA
  '';

  pg-setup = pkgs.writeShellScriptBin "pg-setup" ''
    if ! test -d $PGDATA; then
      pg_ctl initdb -D $PGDATA
      
      if [[ "$PGPORT" ]]; then
        sed -i "s|^#port.*$|port = $PGPORT|" $PGDATA/postgresql.conf
      fi
      
      echo "listen_addresses = ${"'"}${"'"}" >> $PGDATA/postgresql.conf
      echo "unix_socket_directories = '$PGDATA'" >> $PGDATA/postgresql.conf
      echo "CREATE USER postgres WITH PASSWORD 'postgres' CREATEDB SUPERUSER;" | postgres --single -E postgres
    fi
  '';

  pg-start = pkgs.writeShellScriptBin "pg-start" ''
    [ ! -d $PGDATA ] && pg-setup

    HOST_COMMON="host\s\+all\s\+all"
    sed -i "s|^$HOST_COMMON.*127.*$|host all all 0.0.0.0/0 trust|" $PGDATA/pg_hba.conf
    sed -i "s|^$HOST_COMMON.*::1.*$|host all all ::/0 trust|" $PGDATA/pg_hba.conf

    pg_ctl \
      -D $PGDATA \
      -l $PGDATA/postgres.log \
      -o "-c unix_socket_directories='$PGDATA'" \
      -o "-c listen_addresses='*'" \
      -o "-c log_destination='stderr'" \
      -o "-c logging_collector=on" \
      -o "-c log_directory='log'" \
      -o "-c log_filename='postgresql-%Y-%m-%d_%H%M%S.log'" \
      -o "-c log_min_messages=info" \
      -o "-c log_min_error_statement=info" \
      -o "-c log_connections=on" \
      start
  '';

  pg-console = pkgs.writeShellScriptBin "pg-console" ''
    psql --host $PGDATA -U postgres
  '';
}