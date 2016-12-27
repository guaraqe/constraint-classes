# constraint-classes

This package reimplements various type classes for types that have constraints.
Constraints are represented using `ConstraintKinds` and the classes are
constructed in a way that is supposed to not clash with the regular ones.

# Motivation

Many types constructors have fundamental constraints in their domains. For
example, storable vectors can only contain elements that are `Storable`. In
these cases, no class instances can be done and overloading is completely lost.
This package tries to solve this problem while being `Prelude`-compatible, as
seamless to use as possible and preserving type inference. These constraints
make a more general, `subhask`-like solution not desirable.

# Architecture

The general rule is:

- classes are prefixed with a `C`: `Class` becomes `CClass`
- functions are prefixed with a `_`: `function` becomes `_function`
- operators are suffixed with `:`: `*` becomes `*:`

A type family `Dom f a` that represents the `a` that are in the domain of the
type function `f`. The domain can be cartesian or closed, and the classes
methods are adapted to this using classes `DomCartesian` and `DomClosed`
containing the appropriate constraint entailments.

Currently the implemented classes are, approximately from `base`, which some
changes:
- `CFunctor`
- `CApply`
- `CApplicative`
- `CMonad`
- `CAlt`
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

# Problems

An instance for `Apply` for a functor whose domain is not cartesian closed
needs to have `_ap = undefined`. This is not a problem since any use of `_ap`
will not compile due to the lack of `DomClosed` instance, but it's ugly.

Monads are supposed to be endofunctors, which is not the case here, I'm not
sure how to interpret that.
