{
  pkgs ? import <nixpkgs> {}
}:

let
  postgresImage = pkgs.dockerTools.buildImage {
    name = "postgresql-alpine";
    tag = "latest";
    fromImage = pkgs.dockerTools.pullImage {
      imageName = "postgres";
      imageTag = "15-alpine";
    };
    config = {
      Cmd = [ "postgres" ];
      Env = [
        "POSTGRES_USER=cheeblr_user"
        "POSTGRES_PASSWORD=securepassword"
        "POSTGRES_DB=cheeblr"
      ];
      Volumes = {
        "/var/lib/postgresql/data" = {};
      };
    };
  };
in
{
  packages.postgres = postgresImage;
}