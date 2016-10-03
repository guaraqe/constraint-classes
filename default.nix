{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "constraint-classes";
  version = "0.3.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "http://github.com/guaraqe/constraint-classes#readme";
  description = "Prelude classes using ConstraintKinds";
  license = stdenv.lib.licenses.bsd3;
}
