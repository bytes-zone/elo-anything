{ ... }:
let
  sources = import nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  gitignore = import sources.gitignore { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "elo-anything";
  src = gitignore.gitignoreSource ./.;

  buildInputs = [ elmPackages.elm elmPackages.elm-test ];
  buildPhase = ''
    env ELM_HOME=.elm make dist
  '';

  doCheck = true;
  checkPhase = ''
    env ELM_HOME=.elm elm-test
  '';

  installPhase = ''
    mkdir -p $out/share/
    mv dist $out/share/elo-anything
  '';
}
