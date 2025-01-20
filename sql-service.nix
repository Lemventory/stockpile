{ config, pkgs, ... }:

{
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_15;
    settings = {
      listen_addresses = "127.0.0.1"; # Restrict access
      max_connections = 10;
    };
  };

  networking.firewall.allowedTCPPorts = [ 5432 ];
}