# constraint-classes

This package implements some type classes using `ConstraintKinds` in a way that
is supposed to not clash with the `Prelude` ones. The general rule is that
class `Class` becomes `CClass` and the function `function` becomes `_function`.

A single constraint `Con f a` is used by all the classes, and is given by a
type family.

Currently the implemented classes are:
- `CFunctor`
- `CApplicative`
- `CMonad`
- `CAlternative`
- `CFoldable`
- `CTraversable`
- `CZippable`
- `CIndexed`
- `CIndexFunctor`
- `CIndexFoldable`

I use them mainly for `Data.Vector.Storable`, so that `Applicative` and `Monad`
in their standard forms are not implementable, which explains their alternative
forms here, look at the code for more info.
