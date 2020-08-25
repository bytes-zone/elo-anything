{ ... }:
let
  sources = import nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "elo-anything";
  src = ./.;

  buildInputs = [ elmPackages.elm ];
  buildPhase = ''
    env ELM_HOME=.elm make dist
  '';

  installPhase = ''
    mkdir -p $out/share/
    mv dist $out/share/elo-anything
  '';
}
