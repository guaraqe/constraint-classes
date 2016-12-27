{ mkDerivation, base, constraints, ghc-prim, stdenv }:
mkDerivation {
  pname = "constraint-classes";
  version = "0.5";
  src = ./.;
  libraryHaskellDepends = [ base constraints ghc-prim ];
  homepage = "http://github.com/guaraqe/constraint-classes#readme";
  description = "Various typeclasses using ConstraintKinds";
  license = stdenv.lib.licenses.bsd3;
}
