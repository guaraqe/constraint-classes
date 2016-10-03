{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv }:
      mkDerivation {
        pname = "constraint-classes";
        version = "0.3.0";
        src = ./.;
        libraryHaskellDepends = [ base ];
        homepage = "http://github.com/guaraqe/constraint-classes#readme";
        description = "Prelude classes using ConstraintKinds";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
