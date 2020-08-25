{ ... }:
let
  sources = import nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  gitignore = import sources.gitignore { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "elo-anything";
  src = gitignore.gitignoreSource ./.;

  buildInputs = [ elmPackages.elm ];
  buildPhase = ''
    env ELM_HOME=.elm make dist
  '';

  installPhase = ''
    mkdir -p $out/share/
    mv dist $out/share/elo-anything
  '';
}
