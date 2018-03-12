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
  description = "Various typeclasses using ConstraintKinds";
  license = stdenv.lib.licenses.bsd3;
}
