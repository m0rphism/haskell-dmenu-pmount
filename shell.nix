{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, directory, dmenu, lens, mtl
      , process, stdenv, transformers
      }:
      mkDerivation {
        pname = "dmenu-pmount";
        version = "0.1.0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base containers directory dmenu lens mtl process transformers
        ];
        homepage = "https://github.com/m0rphism/haskell-dmenu-pmount";
        description = "Mounting and unmounting linux devices as user with dmenu and pmount";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
