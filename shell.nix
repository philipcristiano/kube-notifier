let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;

  # Get from github
  ghpkgs = pkgs.callPackage (fetchTarball https://github.com/philipcristiano/dev-env/archive/v0.0.7.tar.gz) {};
  # Get local version used for testing
  # ghpkgs = pkgs.callPackage "/Users/philipcristiano/gits/dev-env/default.nix" {};

in stdenv.mkDerivation {
  name = "env";
  buildInputs = [
                  pkgs.erlangR19
                  ghpkgs.kubectl.kubectl_1_6_6
                ];
}
