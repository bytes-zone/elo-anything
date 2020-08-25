{ ... }:
let
  sources = import ./nix/sources.nix;

  nixpkgs = import sources.nixpkgs { };

  niv = import sources.niv { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "elo-anything";
  buildInputs = [
    niv.niv
    git

    # Elm
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-test

    # Build Stuff
    gnumake
    modd
    devd
  ];
}
