{
  description = "prelude.fut";

  nixConfig.bash-prompt = "\[nix-develop\]$ ";

  inputs.nixpkgs.url = "github:nixos/nixpkgs";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { 
          inherit system;
        };
        src = ./.;
        name = "prelude.fut";
        version = "0.0.1";

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
            nativeBuildInputs = [ pkgs.futhark ];
            buildPhase = ''
              mkdir -p $out
              cp -r $src/lib $out
              cp futhark.pkg $out
            '';

            checkPhase = checks.defaultPackage.checkPhase;
          };

          packages.web = pkgs.stdenv.mkDerivation {
            name = "${name}-web";
            inherit src;
            dontInstall = true;
            nativeBuildInputs = [ pkgs.futhark ];
            buildPhase = ''
              mkdir -p $out
              cp -r $src/web/. $out
              chmod -R 755 $out
              futhark doc lib -o $out/docs
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