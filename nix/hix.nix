{pkgs, ...}: {
  name = "cheeblr-backend";
  compiler-nix-name = "ghc964";
  shell.tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
  };
}