{
  description = "prelude.fut";

  nixConfig.bash-prompt = "\[nix-develop\]$ ";

  inputs.nixpkgs.url = "github:nixos/nixpkgs";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.futhark-repo.url = "github:diku-dk/futhark";

  inputs.futhark-repo.flake = false;

  outputs = { self, nixpkgs, flake-utils, futhark-repo }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { 
          inherit system;
        };
        src = ./.;
        name = "prelude.fut";
        version = let _ver = builtins.getEnv "GITVERSION_NUGETVERSIONV2"; in if _ver == "" then "0.0.0" else "${_ver}.${builtins.getEnv "GITVERSION_COMMITSSINCEVERSIONSOURCE"}";

        futharktar = pkgs.callPackage futhark-repo { };
        futhark = pkgs.stdenv.mkDerivation {
            name = "futhark";
            src = "${futharktar}/futhark-nightly.tar.xz";
            installPhase = ''
              mkdir -p $out
              cp ./* -R $out
              ls $out
            '';
        };

      in rec {
          devShell = pkgs.mkShell {
            inherit name;
            inherit version;
            buildInputs = defaultPackage.nativeBuildInputs;
          };
    
          defaultPackage = pkgs.stdenv.mkDerivation {
            inherit name;
            inherit src;
            doCheck = true;
            dontInstall = true;
            # Using nixpkgs futhark is nicer to build with, but it doesn't have LSP support yet
            nativeBuildInputs = [ futhark ];
            buildPhase = ''
              mkdir -p $out
              cp -r $src/lib $out
              cp futhark.pkg $out
            '';

            checkPhase = checks.defaultPackage.checkPhase;
          };

          packages.docs = pkgs.stdenv.mkDerivation {
            name = "${name}-docs";
            inherit src;
            dontInstall = true;
            nativeBuildInputs = [ futhark ];
            buildPhase = ''
              futhark doc lib -o $out
            '';
          };

          checks.defaultPackage = pkgs.stdenv.mkDerivation {
            inherit name;
            inherit src;
            nativeBuildInputs = defaultPackage.nativeBuildInputs;
            doCheck = true;
            phases = [ "unpackPhase" "buildPhase" "checkPhase" ];
            buildPhase = ''
              mkdir -p $out
              touch $out/noop
            '';
            checkPhase = ''
              find . -name "*.actual" -type f -delete
              find . -name "*.expected" -type f -delete

              set +e
              futhark test --backend=c --interpreted --no-terminal lib
              if [ $? -ne 0 ]; then
                  echo "Checks failed"
                  echo "Expected: "
                  for i in *.expected ; do cat "$i" | futhark dataset --text; done
                  echo "Got: "
                  for i in *.actual ; do cat "$i" | futhark dataset --text; done
                  exit 1
              fi
            '';
          };
      }
    );
}