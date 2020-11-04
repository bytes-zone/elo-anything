{ ... }:
let
  sources = import nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  gitignore = import sources.gitignore { };
in rec {
  elo-anything = pkgs.stdenv.mkDerivation {
    name = "elo-anything";
    src = gitignore.gitignoreSource ./.;

    buildInputs = [ pkgs.elmPackages.elm pkgs.elmPackages.elm-test ];
    buildPhase = pkgs.elmPackages.fetchElmDeps {
      elmPackages = import ./nix/elm-srcs.nix;
      elmVersion = "0.19.1";
      registryDat = ./nix/registry.dat;
    };

    doCheck = true;
    checkPhase = ''
      env ELM_HOME=.elm elm-test
    '';

    installPhase = ''
      make dist
      mkdir -p $out/share/
      mv dist $out/share/elo-anything
    '';
  };

  container = let
    linuxPkgs = import sources.nixpkgs { system = "x86_64-linux"; };
    listenPort = "80";
  in linuxPkgs.dockerTools.buildLayeredImage {
    name = "elo-anything";
    contents = linuxPkgs.darkhttpd;
    config = {
      Cmd = [ "darkhttpd" "${elo-anything}/share/elo-anything" ];
      ExposedPorts = { "${listenPort}/tcp" = { }; };
    };
  };
}
