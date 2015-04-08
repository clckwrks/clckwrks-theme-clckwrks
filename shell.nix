with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, clckwrks, containers, happstack-authenticate
             , hsp, hsx2hs, mtl, stdenv, text, web-plugins
             }:
             mkDerivation {
               pname = "clckwrks-theme-clckwrks";
               version = "0.5.0";
               src = ./.;
               buildDepends = [
                 base clckwrks containers happstack-authenticate hsp hsx2hs mtl text
                 web-plugins
               ];
               homepage = "http://www.clckwrks.com/";
               description = "simple bootstrap based template for clckwrks";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
