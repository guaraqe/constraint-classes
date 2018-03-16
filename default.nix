{ mkDerivation, base, constraints, containers, distributive, keys
, linear, stdenv, storable-tuple, transformers, vector, witherable
}:
mkDerivation {
  pname = "constraint-classes";
  version = "0.6.0";
  src = ./.;
  libraryHaskellDepends = [
    base constraints containers distributive keys linear storable-tuple
    transformers vector witherable
  ];
  homepage = "http://github.com/guaraqe/constraint-classes#readme";
  description = "Various typeclasses using ConstraintKinds";
  license = stdenv.lib.licenses.bsd3;
}
