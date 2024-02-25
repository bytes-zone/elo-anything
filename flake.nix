{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        # `nix fmt`
        formatter = pkgs.nixpkgs-fmt;

        # `nix build`
        packages.elo-anything = pkgs.stdenv.mkDerivation {
          name = "elo-anything";
          src = builtins.filterSource (path: type: builtins.match ".+(flake.nix|flake.lock|.github)$" path == null) ./.;

          buildInputs = [ pkgs.elmPackages.elm pkgs.elmPackages.elm-test ];
          buildPhase = pkgs.elmPackages.fetchElmDeps {
            elmPackages = import ./nix/elm-srcs.nix;
            elmVersion = "0.19.1";
            registryDat = ./nix/registry.dat;
          };

          installPhase = ''
            make dist
            mkdir -p $out/share/
            mv dist $out/share/elo-anything
          '';
        };
        defaultPackage = packages.elo-anything;
        overlay = final: prev: { elo-anything = packages.elo-anything; };

        packages.nginx-conf = pkgs.writeTextFile {
          name = "nginx.conf";
          destination = "/etc/nginx/nginx.conf";
          text = ''
            user nobody nobody;
            daemon off;
            error_log /dev/stdout info;
            pid /dev/null;
            events {}

            http {
              include ${pkgs.nginx}/conf/mime.types;
              types_hash_max_size 4096;

              # optimization
              sendfile on;
              tcp_nopush on;
              tcp_nodelay on;
              keepalive_timeout 65;

              gzip on;
              gzip_static on;
              gzip_vary on;
              gzip_comp_level 5;
              gzip_min_length 256;
              gzip_types application/atom+xml application/geo+json application/javascript application/json application/ld+json application/manifest+json application/rdf+xml application/vnd.ms-fontobject application/wasm application/x-rss+xml application/x-web-app-manifest+json application/xhtml+xml application/xliff+xml application/xml font/collection font/otf font/ttf image/bmp image/svg+xml image/vnd.microsoft.icon text/cache-manifest text/calendar text/css text/csv text/javascript text/markdown text/plain text/vcard text/vnd.rim.location.xloc text/vtt text/x-component text/xml;

              access_log /dev/stdout;

              server {
                listen 0.0.0.0:80 default_server;
                listen [::0]:80;
                http2 on;

                root ${packages.elo-anything}/share/elo-anything;

                add_header X-Frame-Options "SAMEORIGIN" always;
                add_header X-Content-Type-Options "nosniff" always;
                add_header X-XSS-Protection "1; mode=block" always;
              }
            }
          '';
        };

        # for debugging, if needed
        # packages.container = pkgs.dockerTools.buildLayeredImage {
        packages.container = pkgs.dockerTools.streamLayeredImage {
          name = "elo-anything";

          # make /var/log/nginx so Nginx doesn't fail trying to open it (which
          # it does no matter what you say in log settings, apparently.
          extraCommands = ''
            mkdir -p tmp/nginx_client_body
            mkdir -p var/log/nginx
          '';

          contents = [
            pkgs.fakeNss
            pkgs.nginxMainline
            packages.nginx-conf

            # for debugging, if needed
            # pkgs.dockerTools.binSh
            # pkgs.coreutils
          ];

          config = {
            "ExposedPorts"."80/tcp" = { };
            Entrypoint = [ "nginx" ];
            Cmd = [ "-c" "/etc/nginx/nginx.conf" ];

            # for debugging, if needed
            # Entrypoint = "/bin/sh";
          };
        };

        # `nix shell`
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
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
        };
      });
}
