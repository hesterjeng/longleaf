{
  description = "Longleaf React frontend";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Development package - provides Node.js environment for npm workflow
        longleaf-frontend-dev = pkgs.stdenv.mkDerivation rec {
          pname = "longleaf-frontend-dev";
          version = "0.1.0";
          
          src = ./.;
          
          buildInputs = with pkgs; [
            nodejs_20
            npm
          ];
          
          buildPhase = ''
            export HOME=$(mktemp -d)
            npm ci --frozen-lockfile
          '';
          
          installPhase = ''
            mkdir -p $out/share/longleaf-frontend
            cp -r . $out/share/longleaf-frontend/
          '';
          
          meta = {
            description = "Longleaf React frontend development environment";
            homepage = "https://github.com/hesterjeng/longleaf";
            license = pkgs.lib.licenses.gpl3Plus;
          };
        };

        # Production package - built static files
        longleaf-frontend = pkgs.stdenv.mkDerivation rec {
          pname = "longleaf-frontend";
          version = "0.1.0";
          
          src = ./.;
          
          buildInputs = with pkgs; [
            nodejs_20
            npm
          ];
          
          buildPhase = ''
            export HOME=$(mktemp -d)
            npm ci --frozen-lockfile
            npm run build
          '';
          
          installPhase = ''
            mkdir -p $out/share/longleaf/static
            cp -r build/* $out/share/longleaf/static/
          '';
          
          meta = {
            description = "Pre-built React dashboard for Longleaf trading platform";
            homepage = "https://github.com/hesterjeng/longleaf";
            license = pkgs.lib.licenses.gpl3Plus;
          };
        };

        # Development shell
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            nodejs_20
            npm
            
            # Development tools
            git
            direnv
          ];
          
          shellHook = ''
            echo "Longleaf React frontend development environment"
            echo "Run 'npm install' to install dependencies"
            echo "Run 'npm start' to start development server"
            echo "Run 'npm run build' to build for production"
          '';
        };

      in {
        packages = {
          default = longleaf-frontend;
          longleaf-frontend = longleaf-frontend;
          longleaf-frontend-dev = longleaf-frontend-dev;
        };
        
        devShells.default = devShell;
        
        # Allow building with nix build
        defaultPackage = longleaf-frontend;
      });
}