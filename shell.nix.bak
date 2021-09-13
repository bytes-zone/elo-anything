{ ... }:
let
  sources = import ./nix/sources.nix;

  nixpkgs = import sources.nixpkgs { };

  niv = import sources.niv { };
in with nixpkgs;
mkShell {
  buildInputs = [
    niv.niv
    git

    # Elm
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-json
    elmPackages.elm-test
    elm2nix

    # Build Stuff
    gnumake
    modd
    devd
  ];
}
