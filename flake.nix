{
  description = "Longleaf algorithmic trading platform - complete system";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    
    # Import component flakes
    frontend.url = "path:./react";
    quantstats.url = "path:./tearsheets";
  };

  outputs = { self, nixpkgs, flake-utils, frontend, quantstats }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Get packages from component flakes
        frontendPackages = frontend.packages.${system};
        quantstatsPackages = quantstats.packages.${system};
        
        # Custom tacaml package - OCaml TA-Lib bindings
        tacaml = pkgs.ocamlPackages.buildDunePackage rec {
          pname = "tacaml";
          version = "0.1.0";
          
          # This would need to be adjusted to point to the actual tacaml source
          # For now, assuming it's a separate repository or local package
          src = builtins.fetchGit {
            url = "https://github.com/hesterjeng/tacaml"; # Adjust URL
            rev = "main"; # Adjust revision
          };
          
          buildInputs = with pkgs.ocamlPackages; [
            dune_3
            ocaml
            pkgs.ta-lib
          ];
          
          nativeBuildInputs = with pkgs; [
            pkg-config
          ];
        };

        # Main longleaf-lib package
        longleaf-lib = pkgs.ocamlPackages.buildDunePackage rec {
          pname = "longleaf-lib";
          version = "1.0.2";
          
          src = ./.;
          
          buildInputs = with pkgs.ocamlPackages; [
            dune_3
            ocaml
            ptime
            ppx_yojson_conv
            ppx_deriving
            ppx_variants_conv
            ppx_fields_conv
            cmdliner
            landmarks
            iter
            containers-data
            ocamlgraph
            eio_main
            piaf
            tacaml
            fileutils
            ocamlformat
            yojson
          ];
          
          nativeBuildInputs = with pkgs; [
            pkg-config
          ];
          
          propagatedBuildInputs = with pkgs; [
            zlib
            openssl
            curl
            libev
            gmp
          ];
          
          checkInputs = with pkgs.ocamlPackages; [
            alcotest
          ];
          
          doCheck = true;
        };

        # Longleaf strategies package
        longleaf-strategies = pkgs.ocamlPackages.buildDunePackage rec {
          pname = "longleaf-strategies";
          version = "1.0.2";
          
          src = ./.;
          
          buildInputs = with pkgs.ocamlPackages; [
            dune_3
            ocaml
            longleaf-lib
          ];
          
          checkInputs = with pkgs.ocamlPackages; [
            alcotest
          ];
          
          doCheck = true;
        };

        # Complete Longleaf package combining all components
        longleaf-complete = pkgs.stdenv.mkDerivation rec {
          pname = "longleaf-complete";
          version = "1.0.2";
          
          dontUnpack = true;
          
          buildInputs = [
            longleaf-lib
            longleaf-strategies
            frontendPackages.longleaf-frontend
            quantstatsPackages.longleaf-quantstats
          ];
          
          installPhase = ''
            mkdir -p $out/bin $out/share
            
            # Link OCaml binaries
            if [ -d "${longleaf-lib}/bin" ]; then
              ln -s ${longleaf-lib}/bin/* $out/bin/ 2>/dev/null || true
            fi
            if [ -d "${longleaf-strategies}/bin" ]; then
              ln -s ${longleaf-strategies}/bin/* $out/bin/ 2>/dev/null || true
            fi
            
            # Link frontend static files
            ln -s ${frontendPackages.longleaf-frontend}/share/longleaf $out/share/
            
            # Link quantstats server
            if [ -d "${quantstatsPackages.longleaf-quantstats}/bin" ]; then
              ln -s ${quantstatsPackages.longleaf-quantstats}/bin/* $out/bin/ 2>/dev/null || true
            fi
          '';
          
          meta = {
            description = "Complete Longleaf algorithmic trading platform";
            homepage = "https://github.com/hesterjeng/longleaf";
            license = pkgs.lib.licenses.gpl3Plus;
          };
        };

        # Development shell with all dependencies from all components
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            # OCaml development
            ocaml
            dune_3
            ocamlformat
            odoc
            
            # Node.js for frontend
            nodejs_20
            npm
            
            # Python for quantstats
            python3
            
            # System dependencies
            pkg-config
            zlib
            openssl
            curl
            libev
            gmp
            ta-lib
            
            # Development tools
            git
            direnv
            tmux
            just
          ] ++ (with pkgs.ocamlPackages; [
            # OCaml packages
            ptime
            ppx_yojson_conv
            ppx_deriving
            ppx_variants_conv
            ppx_fields_conv
            cmdliner
            landmarks
            iter
            containers-data
            ocamlgraph
            eio_main
            piaf
            fileutils
            yojson
            alcotest
            odoc
          ]) ++ (with pkgs.python3Packages; [
            # Python packages for quantstats
            fastapi
            uvicorn
            pydantic
            pandas
            numpy
            frozendict
          ]);
          
          shellHook = ''
            echo "Longleaf complete development environment"
            echo ""
            echo "OCaml backend:"
            echo "  Run 'dune build' to build the project"
            echo "  Run 'dune runtest' to run tests"
            echo ""
            echo "React frontend (cd react/):"
            echo "  Run 'npm install' to install dependencies"
            echo "  Run 'npm start' to start development server"
            echo ""
            echo "Python tearsheets (cd tearsheets/):"
            echo "  Run 'python tearsheet_server.py' to start server"
            echo ""
          '';
        };

        # OCaml-only development shell
        ocamlDevShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            # OCaml development
            ocaml
            dune_3
            ocamlformat
            odoc
            
            # System dependencies
            pkg-config
            zlib
            openssl
            curl
            libev
            gmp
            ta-lib
            
            # Development tools
            git
            direnv
            tmux
            just
          ] ++ (with pkgs.ocamlPackages; [
            # OCaml packages
            ptime
            ppx_yojson_conv
            ppx_deriving
            ppx_variants_conv
            ppx_fields_conv
            cmdliner
            landmarks
            iter
            containers-data
            ocamlgraph
            eio_main
            piaf
            fileutils
            yojson
            alcotest
            odoc
          ]);
          
          shellHook = ''
            echo "Longleaf OCaml development environment"
            echo "Run 'dune build' to build the project"
            echo "Run 'dune runtest' to run tests"
          '';
        };

      in {
        packages = {
          default = longleaf-complete;
          longleaf-complete = longleaf-complete;
          longleaf-lib = longleaf-lib;
          longleaf-strategies = longleaf-strategies;
          tacaml = tacaml;
          
          # Re-export component packages
          inherit (frontendPackages) longleaf-frontend longleaf-frontend-dev;
          inherit (quantstatsPackages) longleaf-quantstats longleaf-quantstats-dev;
        };
        
        devShells = {
          default = devShell;
          ocaml = ocamlDevShell;
          frontend = frontend.devShells.${system}.default;
          quantstats = quantstats.devShells.${system}.default;
        };
        
        # Allow building with nix build
        defaultPackage = longleaf-complete;
      });
}