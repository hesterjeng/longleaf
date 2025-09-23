{
  description = "Longleaf QuantStats tearsheet server";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Custom Python packages that might not be in nixpkgs or need specific versions
        python-multitasking = pkgs.python3Packages.buildPythonPackage rec {
          pname = "multitasking";
          version = "0.0.12";
          
          src = pkgs.fetchPypi {
            inherit pname version;
            sha256 = "sha256-K2QVlOlODLGV8h62EuvgHPbN3+PhlGj1hcZz5JZOOzA=";
          };
          
          # No tests included in source
          doCheck = false;
          
          meta = {
            description = "Non-blocking Python methods using decorators";
            homepage = "https://github.com/ranaroussi/multitasking";
            license = pkgs.lib.licenses.asl20;
          };
        };

        # yfinance pinned to v0.2.57 to avoid curl_cffi dependency
        python-yfinance = pkgs.python3Packages.buildPythonPackage rec {
          pname = "yfinance";
          version = "0.2.57";
          
          src = pkgs.fetchPypi {
            inherit pname version;
            sha256 = "sha256-tW7IwiVnbL/v1I1kHdT7qrQNYjsM6Bx/vAhAEhL2lZQ=";
          };
          
          propagatedBuildInputs = with pkgs.python3Packages; [
            pandas
            numpy
            requests
            lxml
            appdirs
            pytz
            beautifulsoup4
            websockets
            protobuf
            frozendict
            peewee
          ];
          
          # Skip tests that require network access and sanity check for curl_cffi
          doCheck = false;
          pythonImportsCheck = [ "yfinance" ];
          
          meta = {
            description = "Download market data from Yahoo! Finance API";
            homepage = "https://github.com/ranaroussi/yfinance";
            license = pkgs.lib.licenses.asl20;
          };
        };

        # quantstats package
        python-quantstats = pkgs.python3Packages.buildPythonPackage rec {
          pname = "quantstats";
          version = "0.0.75";
          
          src = pkgs.fetchFromGitHub {
            owner = "ranaroussi";
            repo = "quantstats";
            rev = version;
            sha256 = "sha256-UgVe7hcqtc8T1VBHJhXpJNvLDJlv5LsF1GdL+c1nfXs=";
          };
          
          propagatedBuildInputs = with pkgs.python3Packages; [
            pandas
            numpy
            scipy
            matplotlib
            seaborn
            tabulate
            python-dateutil
            packaging
            python-yfinance
            python-multitasking
            ipython
          ];
          
          # Skip tests that require yfinance network access
          doCheck = false;
          pythonImportsCheck = [ "quantstats" ];
          
          meta = {
            description = "Portfolio analytics for quants";
            homepage = "https://github.com/ranaroussi/quantstats";
            license = pkgs.lib.licenses.asl20;
          };
        };

        # Main QuantStats server package
        longleaf-quantstats = pkgs.python3Packages.buildPythonApplication rec {
          pname = "longleaf-quantstats";
          version = "0.1.0";
          
          src = ./.;
          
          propagatedBuildInputs = with pkgs.python3Packages; [
            fastapi
            uvicorn
            pydantic
            pandas
            numpy
            python-quantstats
            frozendict
          ];
          
          # Create a simple setup.py since we don't have pyproject.toml
          preBuild = ''
            cat > setup.py << EOF
from setuptools import setup

setup(
    name="${pname}",
    version="${version}",
    py_modules=["tearsheet_server"],
    install_requires=[
        "fastapi",
        "uvicorn",
        "pydantic",
        "pandas",
        "numpy",
        "quantstats",
        "frozendict",
    ],
    entry_points={
        "console_scripts": [
            "longleaf-quantstats=tearsheet_server:main",
        ],
    },
)
EOF
          '';
          
          doCheck = false;
          
          meta = {
            description = "Longleaf QuantStats FastAPI server for portfolio analytics";
            homepage = "https://github.com/hesterjeng/longleaf";
            license = pkgs.lib.licenses.gpl3Plus;
          };
        };

        # Development package - provides Python environment
        longleaf-quantstats-dev = pkgs.stdenv.mkDerivation rec {
          pname = "longleaf-quantstats-dev";
          version = "0.1.0";
          
          src = ./.;
          
          buildInputs = with pkgs; [
            python3
          ] ++ (with pkgs.python3Packages; [
            python-quantstats
            pandas
            numpy
            frozendict
            fastapi
            uvicorn
            pydantic
          ]);
          
          buildPhase = "true";  # No build needed
          
          installPhase = ''
            mkdir -p $out/share/longleaf-quantstats
            cp -r . $out/share/longleaf-quantstats/
          '';
          
          meta = {
            description = "Development environment for Longleaf QuantStats server";
            homepage = "https://github.com/hesterjeng/longleaf";
            license = pkgs.lib.licenses.gpl3Plus;
          };
        };

        # Development shell
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            python3
            
            # Development tools
            git
            direnv
          ] ++ (with pkgs.python3Packages; [
            # Python packages
            python-quantstats
            pandas
            numpy
            frozendict
            fastapi
            uvicorn
            pydantic
            
            # Development packages
            python-multitasking
            python-yfinance
            ipython
          ]);
          
          shellHook = ''
            echo "Longleaf QuantStats development environment"
            echo "Run 'python tearsheet_server.py' to start the server"
            echo "Server will be available at http://localhost:5000"
          '';
        };

      in {
        packages = {
          default = longleaf-quantstats;
          longleaf-quantstats = longleaf-quantstats;
          longleaf-quantstats-dev = longleaf-quantstats-dev;
          python-multitasking = python-multitasking;
          python-yfinance = python-yfinance;
          python-quantstats = python-quantstats;
        };
        
        devShells.default = devShell;
        
        # Allow building with nix build
        defaultPackage = longleaf-quantstats;
      });
}