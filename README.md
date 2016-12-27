# constraint-classes

This package reimplements various type classes for types that have constraints.
Constraints are represented using `ConstraintKinds` and the classes are
constructed in a way that is supposed to not clash with the regular ones.

# Motivation

Many types constructors have fundamental constraints in their domains. For
example, storable vectors can only contain elements that are `Storable`. In
these cases, no class instances can be done and overloading is completely lost.

This package tries to solve this problem while:

- being compatible with `Prelude`
- being as seamless to use as possible
- preserving type inference

These constraints make a more general, `subhask`-like solution not desirable.

# Architecture

The domain of a type function `f` is represented by a type family `Dom f a` of
kind `Constraint`.  The domain can be cartesian or closed, represented by
classes `DomCartesian` and `DomClosed` containing the appropriate constraint
entailments.

The class functions use the domain:

    _fmap :: (CFunctor f, Dom f a, Dom f b) => (a -> b) -> f a -> f b

and the domain classifiers:

    _zipA :: (CApply f, DomCartesian f, Dom f a, Dom f b) => f a -> f b -> f (a, b)

when necessary.

The general rule of translation from standard classes is:

- classes are prefixed with a `C`: `Class` becomes `CClass`
- functions are prefixed with a `_`: `function` becomes `_function`
- operators are suffixed with `:`: `*` becomes `*:`


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

# TODO

Improve the architecture, better choice of classes, more classes, more
instances.
