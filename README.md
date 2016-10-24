# constraint-classes

This package reimplements various type classes for types that have constraints.
Constraints are represented using `ConstraintKinds` and the classes are
constructed in a way that is supposed to not clash with the regular ones. The
general rule is that class `Class` becomes `CClass` and the function `function`
becomes `_function`.

The type constraint is given by a type family `Con f a` that used by all the
classes.

Currently the implemented classes are, from `base`:
- `CFunctor`
- `CApplicative`
- `CMonad`
- `CAlternative`
- `CFoldable`
- `CTraversable`

From `keys`:
- `CLookup`
- `CIndexable`
- `CKeyed`
- `CZip`
- `CZipWithKey`
- `CFoldableWithKey`
- `CTraversableWithKey`
- `CAdjustable`

From `distributive`:
- `CDistributive`

From `representable`:
- `CRepresentable`

I use them mainly for `Data.Vector.Storable`, so that some classes like
`Applicative` are not implementable in their standard forms, which explains
their alternative forms here, look at the code for more info.
