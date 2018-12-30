{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, clckwrks, containers
      , happstack-authenticate, hsp, hsx2hs, mtl, stdenv, text
      , web-plugins, cabal-install
      }:
      mkDerivation {
        pname = "clckwrks-theme-clckwrks";
        version = "0.5.2.3";
        src = ./.;
        enableSeparateDataOutput = true;
        libraryHaskellDepends = [
          base clckwrks containers happstack-authenticate hsp hsx2hs mtl text
          web-plugins cabal-install
        ];
        homepage = "http://www.clckwrks.com/";
        description = "simple bootstrap based template for clckwrks";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
